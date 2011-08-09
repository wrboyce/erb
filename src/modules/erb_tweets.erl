%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Prints tweets linked to in channels
%% -------------------------------------------------------------------
-module(erb_tweets).
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/0]).
-include_lib("xmerl/include/xmerl.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {reqchanmap}).

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
    application:start(inets),
    ok = gen_server:call({global, erb_router}, {subscribeToUrls, "twitter.com"}),
    State = #state{ reqchanmap=ets:new(web_titler_reqchanmap, [ordered_set]) },
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
handle_cast({url, Url, Data}, State) ->
    UrlTokens = string:tokens(Url, "/#!"),
    [TwitterUrl,TwitterUser] = case UrlTokens of
        [_,_,User] ->
            BaseUrl = "http://api.twitter.com/1/users/show.xml?screen_name=~s",
            [lists:flatten(io_lib:format(BaseUrl, [User])), User];
        [_,_,User,_,TweetId] ->
            BaseUrl = "http://api.twitter.com/1/statuses/show.xml?id=~s",
            [lists:flatten(io_lib:format(BaseUrl, [TweetId])), User]
    end,
    {ok, RequestId} = httpc:request(get, {TwitterUrl,[]}, [], [{sync, false}]),
    error_logger:info_msg("Storing RequestId: ~p (~p)~n", [RequestId, {TwitterUser, Data#data.destination}]),
    true = ets:insert(State#state.reqchanmap, {RequestId, {TwitterUser, Data#data.destination}}),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% -------------------------------------------------------------------
handle_info({http, {RequestId, Response}}, State) ->
    case ets:lookup(State#state.reqchanmap, RequestId) of
        [{RequestId, {TwitterUser, Chan}}] ->
            case Response of
                {{_,200,_}, _Headers, Body} ->
                    BodyString = erlang:binary_to_list(Body),
                    {BodyNode, _} = xmerl_scan:string(BodyString),
                    [StatusTextNode] = xmerl_xpath:string("//status/text", BodyNode),
                    Message = io_lib:format("[twitter.com/~s] ~s", [TwitterUser, (lists:nth(1, StatusTextNode#xmlElement.content))#xmlText.value]),
                    ok = gen_server:cast({global, erb_dispatcher}, {privmsg, Chan, Message});
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
