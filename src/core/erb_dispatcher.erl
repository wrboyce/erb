%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Expose API for common IRC commands, and send data to Connector
%% -------------------------------------------------------------------
-module(erb_dispatcher).
-author("Will Boyce").
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([register/1, start_link/0, privmsg/2, join/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -------------------------------------------------------------------
%% @spec register(Nick) -> ok.
%% @doc Perform initial registration with server
%% -------------------------------------------------------------------
register(Nick) ->
	gen_server:cast(?SERVER, {register, [Nick]}),
	gen_server:cast(?SERVER, {register, [Nick, "localhost", "localhost", "Erb <http://code.google.com/p/erb>"]}).
	
%% -------------------------------------------------------------------
%% @spec privmsg(Dest, Msg) -> ok.
%% @doc Send Msg to Dest(ination) via PRIVMSG command
%% -------------------------------------------------------------------
privmsg(Dest, Msg) ->
	gen_server:cast(?SERVER, {privmsg, [Dest, Msg]}).

%% -------------------------------------------------------------------
%% @spec join(Chans) -> ok.
%% @doc Join channels
%% -------------------------------------------------------------------
join([]) ->
	ok;
join([Chan | Chans]) ->
	gen_server:cast(?SERVER, {join, [Chan]}),
	join(Chans).


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
    {ok, #state{}}.

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
handle_cast({register, [Nick]}, State) ->
	erb_connector:send_line(irc_lib:register(Nick)),
	{noreply, State};
handle_cast({register, [User, Host, Server, RealName]}, State) ->
	erb_connector:send_line(irc_lib:register(User, Host, Server, RealName)),
	{noreply, State};
handle_cast({privmsg, [Dest, Mask]}, State) ->
	erb_connector:send_line(irc_lib:privmsg(Dest, Mask)),
	{noreply, State};
handle_cast({join, Chan}, State) ->
	erb_connector:send_line(irc_lib:join(Chan)),
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
%% Internal functions
%% ===================================================================
