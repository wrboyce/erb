%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Manage the socket to the IRC Server
%% -------------------------------------------------------------------
-module(erb_connector).
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {server, port, sock}).

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
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
    case gen_server:call({global, erb_config_server}, {getConfig, server}) of
        {ok, {Server, Port}} ->
            error_logger:info_msg("Connecting to ~s:~B...", [Server, Port]),
            case gen_tcp:connect(Server, Port, [{packet, line}, {active, true}]) of
                {ok, Sock} ->
                    error_logger:info_msg("~s:~B connected.~n", [Server, Port]),
                    gen_fsm:send_event({global, erb_processor}, connected),
                    {ok, #state{server=Server, port=Port, sock=Sock}};
                _ ->
                    {stop, socket_error}
            end;
        _ ->
            {stop, config_error}
    end.

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
    {ok, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({sendline, Data}, State) ->
    % error_logger:info_msg("TX: ~p~n", [Data]),
    gen_tcp:send(State#state.sock, Data ++ "\r\n"),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    Lines = string:tokens(Data, "\r\n"),
    dispatch(Lines),
    {noreply, State#state{sock=Sock}};
handle_info({tcp_closed, _Sock}, State) ->
    % init([]), (?)
    {noreply, State};
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
%% -------------------------------------------------------------------
%% @private
%% @spec dispatch(Lines) -> ok
%% @doc Dispatch lines to the router
%% -------------------------------------------------------------------
dispatch([]) ->
    ok;
dispatch([Line|Lines]) ->
    % error_logger:info_msg("RX: ~p~n", [Line]),
    gen_fsm:send_event({global, erb_processor}, {recv, Line}),
    dispatch(Lines).
