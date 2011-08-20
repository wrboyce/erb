%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Routes data from the server to registered modules
%% -------------------------------------------------------------------
-module(erb_router).
-behaviour(gen_server).

%% Include Files
-include_lib("stdlib/include/qlc.hrl").
-include("erb.hrl").
-compile(export_all).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {}).
-record(subscription, {type, arg, pid}).

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
    mnesia:create_table(subscription, [{type, bag}, {ram_copies, [node()]}, {attributes, record_info(fields, subscription)}]),
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
handle_call({subscribeToOperation, Operation}, {FromPid,_}, State) when is_atom(Operation) ->
    ok = add_subscription({operation, Operation}, FromPid),
    {reply, ok, State};
handle_call({subscribeToCommand, Command}, {FromPid,_}, State) when is_atom(Command) ->
    ok = add_subscription({command, Command}, FromPid),
    {reply, ok, State};
handle_call({subscribeToUrls, Domain}, {FromPid,_}, State) ->
    ok = add_subscription({url, Domain}, FromPid),
    {reply, ok, State};
handle_call(subscribeToUnclaimedUrls, {FromPid,_}, State) ->
    ok = add_subscription({url, catchall}, FromPid),
    {reply, ok, State};
handle_call(subscribeToAllUrls, {FromPid,_}, State) ->
    ok = add_subscription({url, all}, FromPid),
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
    OperationPids = get_subscribed_pids({operation, Data#data.operation}),
    lists:map(fun(OperationPid) ->
        gen_server:cast(OperationPid, {event, Data})
    end, OperationPids),
    case Data#data.operation of
        privmsg ->
            case Data#data.body of
                "." ->
                    gen_server:cast({global, erb_dispatcher}, {privmsg, Data#data.destination, "Pong!"});
                _ ->
                    % Check if the data is a command and send to any subscribed modules
                    case string:substr(Data#data.body, 1, 1) of
                        "." ->
                            CommandArgs = string:substr(Data#data.body, 2),
                            [CommandString|Args] = string:tokens(CommandArgs, " "),
                            Command = list_to_atom(CommandString),
                            CommandData = Data#data{ operation = Command, body = Args },
                            error_logger:info_msg("[command] operation=~p args=~p~n", [Command, Args]),
                            CommandPids = get_subscribed_pids({command, CommandData#data.operation}),
                            error_logger:info_msg("subscribers=~p~n", [CommandPids]),
                            lists:map(fun(Pid) ->
                                gen_server:cast(Pid, {Command, CommandData})
                            end, CommandPids);
                        _ ->
                            Words = string:tokens(Data#data.body, " "),
                            Urls = lists:filter(fun(Word) ->
                                case string:to_lower(string:substr(Word, 1, 4)) of
                                    "http" ->
                                        true;
                                    _ ->
                                        false
                                end
                            end, Words),
                            lists:map(fun(Url) ->
                                error_logger:info_msg("Got URL: ~s~n", [Url]),
                                [_Protocol,Domain|_Path] = string:tokens(Url, "/"),
                                AllDomainPids = get_subscribed_pids({url, all}),
                                DomainPids = case get_subscribed_pids({url, string:to_lower(Domain)}) of
                                    [] ->
                                        get_subscribed_pids({url, catchall}) ++ AllDomainPids;
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
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ok = del_subscriptions(Pid),
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
add_subscription({Type, Arg}, Pid) ->
    erlang:monitor(process, Pid),
    error_logger:info_msg("add_subscription({~p, ~p}, ~p)~n", [Type, Arg, Pid]),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#subscription{ type = Type, arg = Arg, pid = Pid })
    end),
    ok.

get_subscribed_pids({Type, Arg}) ->
    Q = qlc:q([S#subscription.pid || S <- mnesia:table(subscription),
            S#subscription.type =:= Type,
            S#subscription.arg =:= Arg]),
    {atomic, Pids} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Pids.

del_subscriptions(Pid) ->
    Q = qlc:q([S || S <- mnesia:table(subscription),
            S#subscription.pid =:= Pid]),
    {atomic, ok} = mnesia:transaction(fun() ->
        lists:map(fun(S) ->
            mnesia:delete_object(S)
        end, qlc:e(Q)),
        ok
    end),
    ok.
