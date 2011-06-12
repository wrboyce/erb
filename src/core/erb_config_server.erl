%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Erb Configuration Server
%% -------------------------------------------------------------------
-module(erb_config_server).
-author("Will Boyce").
-behaviour(gen_server).
-include("erb.hrl").

%% Include Files
-include_lib("stdlib/include/qlc.hrl").

%% Exported functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% Records
-record(state, {}).
-record(config, {service, config}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


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
init([]) ->
    application:start(mnesia),
    mnesia:create_schema([node()]),
    mnesia:create_table(config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, config)}]),
    {ok, #state{}}.

%% -------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% -------------------------------------------------------------------
handle_call({getConfig, Service}, _From, State) ->
    error_logger:info_msg("Retrieving configuration: ~p", [Service]),
    Q = qlc:q([
        C#config.config || C <- mnesia:table(config),
            C#config.service =:= Service
    ]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Reply = case Result of
        [Config] ->
            error_logger:info_msg("Got configuration: ~p = ~p~n", [Service, Config]),
            {ok, Config};
        [] ->
            error_logger:warning_msg("No configuration found: ~p~n", [Service]),
            noconfig
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({putConfig, Service, Config}, State) ->
    error_logger:info_msg("Setting configuration: ~p = ~p~n", [Service, Config]),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(config, #config{service=Service, config=Config}, write)
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% -------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% -------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% -------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%%  Internal functions
%% ===================================================================
