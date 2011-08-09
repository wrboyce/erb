%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Shows the title of URLs not claimed by other plugins
%% -------------------------------------------------------------------
-module(erb_web_titles).
-behaviour(gen_server).
-include("erb.hrl").

%% API
-export([start_link/0]).

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
    ok = gen_server:call({global, erb_router}, subscribeToUnclaimedUrls),
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
    {ok, RequestId} = httpc:request(get, {Url,[]}, [], [{sync, false}]),
    error_logger:info_msg("Storing RequestId: ~p (~p)~n", [RequestId, {Url, Data#data.destination}]),
    true = ets:insert(State#state.reqchanmap, {RequestId, {Url, Data#data.destination}}),
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
        [{RequestId, {Url, Chan}}] ->
            case Response of
                {{_,200,_}, Headers, Body} ->
                    [{_, ContentType}] = lists:filter(fun({K,_V}) ->
                        string:equal(string:to_lower(K), "content-type")
                    end, Headers),
                    case string:str(string:to_lower(ContentType), "html") of
                        0 ->
                            pass;
                        _ ->
                            {match, [Title]} = re:run(Body, "<title>(.*?)</title>", [caseless,multiline,{capture,[1],list}]),
                            [_Proto, Host|_Path] = string:tokens(Url, "/"),
                            ok = gen_server:cast({global, erb_dispatcher}, {privmsg, Chan, "[" ++ Host ++ "] " ++ Title}),
                            ok
                    end;
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
