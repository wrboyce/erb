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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% Bot record
-record(subscription, {bot, type, arg, pid}).

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link(Bot) ->
    gen_server:start_link(Bot#bot.router, ?MODULE, [Bot], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec init(Args) -> {ok, Bot} |
%%                         {ok, Bot, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% -------------------------------------------------------------------
init([Bot]) ->
    mnesia:create_table(subscription, [{type, bag}, {ram_copies, [node()]}, {attributes, record_info(fields, subscription)}]),
    {ok, Bot}.

%% -------------------------------------------------------------------
%% @spec handle_call(Request, From, Bot) -> {reply, Reply, Bot} |
%%                                      {reply, Reply, Bot, Timeout} |
%%                                      {noreply, Bot} |
%%                                      {noreply, Bot, Timeout} |
%%                                      {stop, Reason, Reply, Bot} |
%%                                      {stop, Reason, Bot}
%% @doc Handling call messages
%% -------------------------------------------------------------------
handle_call({subscribeToOperation, Operation}, {FromPid,_}, Bot) when is_atom(Operation) ->
    ok = add_subscription(Bot, {operation, Operation}, FromPid),
    {reply, ok, Bot};
handle_call({subscribeToCommand, Command}, {FromPid,_}, Bot) when is_atom(Command) ->
    ok = add_subscription(Bot, {command, Command}, FromPid),
    {reply, ok, Bot};
handle_call({subscribeToUrls, Domain}, {FromPid,_}, Bot) ->
    ok = add_subscription(Bot, {url, Domain}, FromPid),
    {reply, ok, Bot};
handle_call(subscribeToUnclaimedUrls, {FromPid,_}, Bot) ->
    ok = add_subscription(Bot, {url, catchall}, FromPid),
    {reply, ok, Bot};
handle_call(subscribeToAllUrls, {FromPid,_}, Bot) ->
    ok = add_subscription(Bot, {url, all}, FromPid),
    {reply, ok, Bot};
handle_call(_Request, _From, Bot) ->
    {noreply, Bot}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, Bot) -> {noreply, Bot} |
%%                                      {noreply, Bot, Timeout} |
%%                                      {stop, Reason, Bot}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({data, Data}, Bot) ->
    % Send to modules subscribed to the raw irc operation
    OperationPids = get_subscribed_pids(Bot, {operation, Data#data.operation}),
    lists:map(fun(OperationPid) ->
        gen_server:cast(OperationPid, {event, Data})
    end, OperationPids),
    case Data#data.operation of
        privmsg ->
            case Data#data.body of
                "." ->
                    gen_server:cast(Bot#bot.dispatcher, {privmsg, Data#data.destination, "Pong!"});
                _ ->
                    % Check if the data is a command and send to any subscribed modules
                    case string:substr(Data#data.body, 1, 1) of
                        "." ->
                            CommandArgs = string:substr(Data#data.body, 2),
                            [CommandString|Args] = string:tokens(CommandArgs, " "),
                            Command = list_to_atom(CommandString),
                            CommandData = Data#data{ operation = Command, body = Args },
                            error_logger:info_msg("[command] operation=~p args=~p~n", [Command, Args]),
                            CommandPids = get_subscribed_pids(Bot, {command, CommandData#data.operation}),
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
                                AllDomainPids = get_subscribed_pids(Bot, {url, all}),
                                DomainPids = case get_subscribed_pids(Bot, {url, string:to_lower(Domain)}) of
                                    [] ->
                                        get_subscribed_pids(Bot, {url, catchall}) ++ AllDomainPids;
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
    {noreply, Data#data.bot};
handle_cast(_Request, Bot) ->
    {noreply, Bot}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, Bot) -> {noreply, Bot} |
%%                                       {noreply, Bot, Timeout} |
%%                                       {stop, Reason, Bot}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, _Reason}, Bot) ->
    ok = del_subscriptions(Pid),
    {noreply, Bot};
handle_info(_Reqest, Bot) ->
    {noreply, Bot}.

%% -------------------------------------------------------------------
%% @spec terminate(Reason, Bot) -> ok
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% -------------------------------------------------------------------
terminate(_Reason, _Bot) ->
    ok.

%% -------------------------------------------------------------------
%% @spec code_change(OldVsn, Bot, Extra) -> {ok, NewBot}
%% @doc Convert process state when code is changed
%% -------------------------------------------------------------------
code_change(_OldVsn, Bot, _Extra) ->
    {ok, Bot}.


%% ===================================================================
%% Internal functions
%% ===================================================================
add_subscription(Bot, {Type, Arg}, Pid) ->
    erlang:monitor(process, Pid),
    error_logger:info_msg("add_subscription(~p, {~p, ~p}, ~p)~n", [Bot, Type, Arg, Pid]),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#subscription{ bot=Bot#bot.id, type=Type, arg=Arg, pid=Pid })
    end),
    ok.

get_subscribed_pids(Bot, {Type, Arg}) ->
    Q = qlc:q([S#subscription.pid || S <- mnesia:table(subscription),
            S#subscription.bot =:= Bot#bot.id,
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
