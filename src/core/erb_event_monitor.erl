%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Monitor the connection between the event manager and an event handler
%% -------------------------------------------------------------------
-module(erb_event_monitor).
-author("Will Boyce").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% state record
-record(state, {handler}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link(Handler) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link(Handler) ->
	gen_server:start_link(?MODULE, Handler, []).
	
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
init(Handler) ->
	io:format("Attaching handler ~p to erb_router~n", [Handler]),
	ok = gen_event:add_sup_handler(erb_router, Handler, []),
    {ok, #state{handler=Handler}}.

%% -------------------------------------------------------------------
%% @spec %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% -------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    %% gen_event manager sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
    io:format("~w: detected handler ~p shutdown:~n~p~n",
              [?MODULE, Handler, Reason]),
    {stop, {handler_died, Handler, Reason}, State};
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
%% Internal functions
%% ===================================================================
