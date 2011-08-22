%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Prints the users most recently scrobbled track
%% -------------------------------------------------------------------
-module(erb_lastfm).
-author("Will Boyce").
-behaviour(gen_server).

% Include files
-include("erb.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {bot, reqchanmap}).
%% LastFM Aliases (nick->username)
-record(lastalias, {nick, user}).

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
    ok = gen_server:call(Bot#bot.router, {subscribeToCommand, last}),
    ok = gen_server:call(Bot#bot.router, {subscribeToCommand, lastalias}),
    mnesia:create_table(lastalias, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, lastalias)}]),
    State = #state{
        bot=Bot,
        reqchanmap=ets:new(reqchanmap_ets, [private, ordered_set])
    },
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
handle_cast({last, Data}, State) ->
    User = case Data#data.body of
        [] ->
            [Nick|_Rest] = string:tokens(Data#data.origin, "!"),
            Q = qlc:q([
                R#lastalias.user || R <- mnesia:table(lastalias),
                    R#lastalias.nick =:= Nick
            ]),
            {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
            case Result of
                [UserAlias] ->
                    UserAlias;
                [] ->
                    Nick
            end;
        [UserArg|_] ->
            UserArg
    end,
    {ok, RequestId} = httpc:request(get, {"http://ws.audioscrobbler.com/2.0/user/" ++ User ++ "/recenttracks.xml", []}, [], [{sync, false}]),
    error_logger:info_msg("[erb_lastfm] Storing RequestId: ~p (~p)~n", [RequestId, {User, Data#data.destination}]),
    true = ets:insert(State#state.reqchanmap, {RequestId, {User, Data#data.destination}}),
    {noreply, State};
handle_cast({lastalias, Data}, State) ->
    [User|_] = Data#data.body,
    Nick = lists:nth(1, string:tokens(Data#data.origin, "!")),
    R = #lastalias{ nick=Nick, user=User },
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(R) end),
    gen_server:cast(self(), {last, Data}),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info({http, {RequestId, Response}}, State) -> {noreply, State}
%% @doc Callback message received from an asyncronous httpc:request
%% -------------------------------------------------------------------
handle_info({http, {RequestId, Response}}, State) ->
    io:format("Processing RequestId: ~p", [RequestId]),
    case ets:lookup(State#state.reqchanmap, RequestId) of
        [{RequestId, {User, Chan}}] ->
            case Response of
                {{_,200,_}, _Headers, Body} ->
                    BodyString = erlang:binary_to_list(Body),
                    {BodyNode, _} = xmerl_scan:string(BodyString),
                    [RecentTrackNode] = xmerl_xpath:string("//track[1]", BodyNode),
                    [RecentTrackArtistNode] = xmerl_xpath:string("//artist", RecentTrackNode),
                    [RecentTrackNameNode] = xmerl_xpath:string("//name", RecentTrackNode),
                    Message = io_lib:format("[http://last.fm/user/~s] ~s - ~s", [User,
                        (lists:nth(1, RecentTrackArtistNode#xmlElement.content))#xmlText.value,
                        (lists:nth(1, RecentTrackNameNode#xmlElement.content))#xmlText.value]),
                    ok = gen_server:cast((State#state.bot)#bot.dispatcher, {privmsg, Chan, Message});
                _ ->
                    error
            end,
            true = ets:delete(State#state.reqchanmap, RequestId),
            ok;
        [] ->
            error_logger:warning_msg("~s Got unknown http response: ~p~n", [?SERVER, RequestId]),
            error
    end,
    {noreply, State};

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
