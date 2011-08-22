%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Expose API for common IRC commands, and send data to Connector
%% -------------------------------------------------------------------
-module(erb_dispatcher).
-author("Will Boyce").
-behaviour(gen_server).

%% Include files.
-include("erb.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {connector}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link(Bot, Connector) ->
    error_logger:info_msg("I AM YOUR MOOSE"),
    gen_server:start_link(Bot#bot.dispatcher, ?MODULE, [Connector], []).

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
init([Connector]) ->
    {ok, #state{ connector=Connector }}.

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
handle_cast({pong, Server}, State) ->
    ok = gen_server:cast(State#state.connector, {sendline, irc_lib:pong(Server)}),
    {noreply, State};

handle_cast({register, Nick}, State) ->
    error_logger:info_msg("Registering with server as ~s...~n", [Nick]),
    ok = gen_server:cast(State#state.connector, {sendline, irc_lib:register(Nick)}),
    ok = gen_server:cast(State#state.connector, {sendline, irc_lib:register(Nick, "localhost", "localhost", "Erb [http://github.com/wrboyce/erb]")}),
    {noreply, State};

handle_cast({nick, Nick}, State) ->
    gen_server:cast(State#state.connector, {sendline, irc_lib:register(Nick)}),
    {noreply, State};

handle_cast({whois, Nick}, State) ->
    gen_server:cast(State#state.connector, {sendline, irc_lib:whois(Nick)}),
    {noreply, State};

handle_cast({privmsg, Dest, Message}, State) ->
    gen_server:cast(State#state.connector, {sendline, irc_lib:privmsg(Dest, Message)}),
    {noreply, State};

handle_cast({kick, Dest, Nick, Reason}, State) ->
    gen_server:cast(State#state.connector, {sendline, irc_lib:kick(Dest, Nick, Reason)}),
    {noreply, State};

handle_cast({join, Chans}, State) ->
    error_logger:info_msg("Joining chans: ~s~n", [string:join(Chans, ", ")]),
    ok = gen_server:cast(State#state.connector, {sendline, irc_lib:join(Chans)}),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:warning_msg("[erb_dispatcher] unknown cast: ~p~n", [Msg]),
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
