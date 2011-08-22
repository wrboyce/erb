%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Handles the initial loading and dynamic (un)loading of modules
%% -------------------------------------------------------------------
-module(erb_module_manager).
-author("Will Boyce").
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/1, gen_spec/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {bot, supervisor, dispatcher, modules, admins}).

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link(Bot) ->
    ProcName = list_to_atom("erb_module_manager_" ++ atom_to_list(Bot#bot.id)),
    gen_server:start_link({global, ProcName}, ?MODULE, Bot, []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% -------------------------------------------------------------------
init(Bot) ->
    Modules = case gen_server:call({global, erb_config_server}, {getConfig, Bot, modules}) of
        {ok, Mods} ->
            Mods;
        noconfig ->
            []
    end,
    Admins = case gen_server:call({global, erb_config_server}, {getConfig, Bot, admins}) of
        {ok, Adms} ->
            Adms;
        noconfig ->
            []
    end,
    gen_server:call(Bot#bot.router, {subscribeToCommand, modules}),
    gen_server:call(Bot#bot.router, {subscribeToCommand, loadmod}),
    gen_server:call(Bot#bot.router, {subscribeToCommand, unloadmod}),
    gen_server:call(Bot#bot.router, {subscribeToCommand, reloadmod}),
    Supervisor = {global, list_to_atom("erb_module_supervisor_" ++ atom_to_list(Bot#bot.id))},
    State = #state{ bot=Bot, supervisor=Supervisor, dispatcher=Bot#bot.dispatcher, modules=sets:from_list(Modules), admins=Admins },
    {ok, State}.

%% -------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% -------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({modules, Data}, State) ->
    ModuleStrings = lists:map(fun(Module) -> atom_to_list(Module) end, sets:to_list(State#state.modules)),
    Message = io_lib:format("[modules] ~s", [string:join(ModuleStrings, ", ")]),
    gen_server:cast(State#state.dispatcher, {privmsg, Data#data.destination, Message}),
    {noreply, State};
handle_cast({loadmod, Data}, State) ->
    UserId = Data#data.account,
    NewState = case lists:member(UserId, State#state.admins) of
        true ->
            [ModuleString|_] = Data#data.body,
            Module = list_to_atom(ModuleString),
            case load_mod(State, Module, State#state.modules) of
                {ok, NewModules} ->
                    Message = io_lib:format("[loadmod] module \"~s\" loaded.", [Module]),
                    gen_server:cast(State#state.dispatcher, {privmsg, Data#data.destination, Message}),
                    State#state{ modules = NewModules };
                {error, _} ->
                    State
            end;
        false ->
            State
    end,
    {noreply, NewState};
handle_cast({unloadmod, Data}, State) ->
    UserId = Data#data.account,
    NewState = case lists:member(UserId, State#state.admins) of
        true ->
            [ModuleString|_] = Data#data.body,
            Module = list_to_atom(ModuleString),
            case unload_mod(State, Module, State#state.modules) of
                {ok, NewModules} ->
                    Message = io_lib:format("[unloadmod] module \"~s\" unloaded.", [Module]),
                    gen_server:cast(State, {privmsg, Data#data.destination, Message}),
                    State#state{ modules = NewModules };
                {error, _Reason} ->
                    State
            end;
        false ->
            State
    end,
    {noreply, NewState};
handle_cast({reloadmod, Data}, State) ->
    UserId = Data#data.account,
    NewState = case lists:member(UserId, State#state.admins) of
        true ->
            [ModuleString|_] = Data#data.body,
            Module = list_to_atom(ModuleString),
            TransientState = case unload_mod(State, Module, State#state.modules) of
                {ok, NewModules} ->
                    UnloadMessage = io_lib:format("[reloadmod] module \"~s\" unloaded.", [Module]),
                    gen_server:cast(State#state.dispatcher, {privmsg, Data#data.destination, UnloadMessage}),
                    State#state{ modules = NewModules };
                {error, not_loaded} ->
                    State
            end,
            case load_mod(State, Module, TransientState#state.modules) of
                {ok, NewerModules} ->
                    LoadMessage = io_lib:format("[reloadmod] module \"~s\" loaded.", [Module]),
                    gen_server:cast(State#state.dispatcher, {privmsg, Data#data.destination, LoadMessage}),
                    State#state{ modules = NewerModules };
                {error, already_loaded} ->
                    TransientState
            end;
        false ->
            State
    end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info(_Reqest, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec terminate(Reason, State) -> ok
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% -------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% -------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% -------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
gen_spec(M,B) ->
    {M, {M, start_link, [B]}, permanent, 2000, worker, [M]}.

unload_mod(State, Module, Modules) ->
    case sets:is_element(Module, Modules) of
        true ->
            supervisor:terminate_child(State#state.supervisor, Module),
            supervisor:delete_child(State#state.supervisor, Module),
            NewModules = sets:del_element(Module, Modules),
            gen_server:cast({global, erb_config_server}, {putConfig, State#state.bot, modules, sets:to_list(NewModules)}),
            {ok, NewModules};
        false ->
            {error, not_loaded}
    end.

load_mod(State, Module, Modules) ->
    case sets:is_element(Module, Modules) of
        true ->
            {error, already_loaded};
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    ChildSpec = erb_module_manager:gen_spec(Module, State#state.bot),
                    supervisor:start_child(State#state.supervisor, ChildSpec),
                    NewModules = sets:add_element(Module, Modules),
                    gen_server:cast({global, erb_config_server}, {putConfig, State#state.bot, modules, sets:to_list(NewModules)}),
                    {ok, NewModules};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
