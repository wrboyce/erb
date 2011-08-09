%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Routes data from the server to registered modules
%% -------------------------------------------------------------------
-module(erb_router).
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {subscriptions}).

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
    State = #state{ subscriptions = ets:new(subscriptions_ets, [bag]) },
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
handle_call({subscribeToOperation, Operation}, {FromPid,_}, State) when is_atom(Operation) ->
    true = ets:insert(State#state.subscriptions, {Operation, FromPid}),
    {reply, ok, State};
handle_call({subscribeToCommand, Command}, {FromPid,_}, State) when is_atom(Command) ->
    error_logger:info_msg("subscribeToCommand ~p from ~p~n", [Command, FromPid]),
    true = ets:insert(State#state.subscriptions, {{cmd, Command}, FromPid}),
    {reply, ok, State};
handle_call({subscribeToUrls, Domain}, {FromPid,_}, State) ->
    true = ets:insert(State#state.subscriptions, {{url, string:to_lower(Domain)}, FromPid}),
    {reply, ok, State};
handle_call(subscribeToUnclaimedUrls, {FromPid,_}, State) ->
    error_logger:info_msg("subscribeToUnclaimedUrls from ~p~n", [FromPid]),
    true = ets:insert(State#state.subscriptions, {{url, catchall}, FromPid}),
    {reply, ok, State};
handle_call(subscribeToAllUrls, {FromPid,_}, State) ->
    true = ets:insert(State#state.subscriptions, {{url, all}, FromPid}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({data, Data}, State) ->
    % Send to modules subscribed to the raw irc operation
    Pids = pids_from_ets(ets:lookup(State#state.subscriptions, Data#data.operation)),
    lists:map(fun(Pid) ->
        gen_server:cast(Pid, {event, Data})
    end, Pids),
    case Data#data.operation of
        privmsg ->
            case Data#data.body of
                "." ->
                    pong;
                _ ->
                    % Check if the data is a command and send to any subscribed modules
                    case string:substr(Data#data.body, 1, 1) of
                        "." ->
                            CommandArgs = string:substr(Data#data.body, 2),
                            [CommandString|Args] = string:tokens(CommandArgs, " "),
                            Command = list_to_atom(CommandString),
                            CommandData = Data#data{ operation = Command, body = Args },
                            error_logger:info_msg("[command] operation=~p args=~p~n", [Command, Args]),
                            CommandPids = pids_from_ets(ets:lookup(State#state.subscriptions, {cmd, CommandData#data.operation})),
                            error_logger:info_msg("subscribers=~p~n", [CommandPids]),
                            lists:map(fun(Pid) ->
                                gen_server:cast(Pid, {Command, CommandData})
                            end, CommandPids);
                        _ ->
                            Words = string:tokens(Data#data.body, " "),
                            Urls = lists:filter(fun(Word) ->
                                case string:to_lower(string:substr(Word, 1, 4)) of
                                    "http" ->
                                        error_logger:info_msg("We got a match!~n"),
                                        true;
                                    _ ->
                                        false
                                end
                            end, Words),
                            lists:map(fun(Url) ->
                                error_logger:info_msg("Got URL: ~s~n", [Url]),
                                [_Protocol,Domain|_Path] = string:tokens(Url, "/"),
                                AllDomainPids = pids_from_ets(ets:lookup(State#state.subscriptions, {url, all})),
                                DomainPids = case pids_from_ets(ets:lookup(State#state.subscriptions, {url, string:to_lower(Domain)})) of
                                    [] ->
                                        pids_from_ets(ets:lookup(State#state.subscriptions, {url, catchall})) ++ AllDomainPids;
                                    Other ->
                                        Other ++ AllDomainPids
                                end,
                                error_logger:info_msg("[erb_router] Got subscribers=~p for {url, domain=~s}~n", [DomainPids, Domain]),
                                lists:map(fun(Pid) ->
                                    gen_server:cast(Pid, {url, Url, Data})
                                end, DomainPids)
                            end, Urls)
                    end
            end;
        _ ->
            pass
    end,
    {noreply, State};
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
pids_from_ets(Results) ->
    lists:map(fun({_Key,Pid}) -> Pid end, Results).
