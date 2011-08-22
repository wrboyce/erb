%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Simple seen database
%% -------------------------------------------------------------------
-module(erb_seen).
-author("Will Boyce").
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {bot, db}).

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the server
%% -------------------------------------------------------------------
start_link(Bot) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Bot], []).


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
init([Bot]) ->
    ok = gen_server:call(Bot#bot.router, {subscribeToOperation, join}),
    ok = gen_server:call(Bot#bot.router, {subscribeToOperation, privmsg}),
    ok = gen_server:call(Bot#bot.router, {subscribeToOperation, part}),
    ok = gen_server:call(Bot#bot.router, {subscribeToCommand, seen}),
    State = #state{ bot=Bot, db=ets:new(seen_ets, [set, private]) },
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
handle_cast({event, Data}, State) ->
    case string:str(Data#data.origin, "!") > 0 of
        true ->
            [Nick|_] = string:tokens(Data#data.origin, "!"),
            Key = {string:to_lower(Nick), Data#data.destination},
            ets:insert(State#state.db, {Key, Data});
        false ->
            pass
    end,
    {noreply, State};
handle_cast({seen, Data}, State) ->
    [Nick|_] = Data#data.body,
    Key = {string:to_lower(Nick), Data#data.destination},
    Response = (io_lib:format("~s: ", [Nick]) ++
        case ets:lookup(State#state.db, Key) of
            [{_,SeenData}] ->
                {SeenAgeDays, {SeenAgeHours,SeenAgeMinutes,SeenAgeSeconds}} = calendar:time_difference(SeenData#data.datetime, calendar:universal_time()),
                io_lib:format("~s was last seen", [Nick]) ++
                (string:join("", lists:map(fun({V,L}) ->
                    case V of
                        0 ->
                            "";
                        _ ->
                            io_lib:format(" ~p ~s", [V, L])
                    end
                end, [{SeenAgeDays, "days"}, {SeenAgeHours, "hours"}, {SeenAgeMinutes, "minutes"}])) ++ io_lib:format(" ~p seconds ago, ", [SeenAgeSeconds]) ++
                case SeenData#data.operation of
                    join -> io_lib:format("joining ~s.", [SeenData#data.destination]);
                    privmsg -> io_lib:format("saying \"~s\".", [SeenData#data.body]);
                    part -> io_lib:format("parting ~s.", [SeenData#data.destination])
                end);
            [] ->
                io_lib:format("I haven't seen ~s.", [Nick])
        end),
    ok = gen_server:cast((State#state.bot)#bot.dispatcher, {privmsg, Data#data.destination, Response}),
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
