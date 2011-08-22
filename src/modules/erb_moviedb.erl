%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Uses themoviedb.org to provide information on movies via .movie
%% -------------------------------------------------------------------
-module(erb_moviedb).
-author("Will Boyce").
-behaviour(gen_server).

% Include files
-include("erb.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([set_apikey/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {bot, apikey, reqchanmap}).

%% ===================================================================
%% API
%% ===================================================================
set_apikey(Key) ->
    gen_server:cast({global, erb_config_server}, {putGlobalConfig, moviedb_apikey, Key}).

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
    case gen_server:call({global, erb_config_server}, {getGlobalConfig, moviedb_apikey}) of
        {ok, ApiKey} ->
            application:start(inets),
            State = #state{
                bot=Bot,
                apikey=ApiKey,
                reqchanmap=ets:new(moviedb_reqchanmap, [private, ordered_set])
            },
            ok = gen_server:call(Bot#bot.router, {subscribeToCommand, movie}),
            {ok, State};
        noconfig ->
            {stop, noApiKey}
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
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% -------------------------------------------------------------------
handle_cast({movie, Data}, State) ->
    SearchString = string:join(Data#data.body, ["+"]),
    Url = io_lib:format("http://api.themoviedb.org/2.1/Movie.search/en/xml/~s/~s", [State#state.apikey, SearchString]),
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    error_logger:info_msg("[erb_moviedb] Storing RequestId: ~s -> ~p (~p)~n", [Url, RequestId, Data#data.destination]),
    true = ets:insert(State#state.reqchanmap, {RequestId, Data#data.destination}),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @spec handle_info({http, {RequestId, Response}}, State) -> {noreply, State}
%% @doc Callback message received from an asyncronous httpc:request
%% -------------------------------------------------------------------
handle_info({http, {RequestId, Response}}, State) ->
    io:format("[erb_moviedb] Processing RequestId: ~p", [RequestId]),
    case ets:lookup(State#state.reqchanmap, RequestId) of
        [{RequestId, Chan}] ->
            case Response of
                {{_,200,_}, _Headers, Body} ->
                    BodyString = erlang:binary_to_list(Body),
                    {BodyNode, _} = xmerl_scan:string(BodyString),
                    [MovieNode|_] = xmerl_xpath:string("//movie", BodyNode),
                    [#xmlText{value=MovieTitle}] = xmerl_xpath:string("/movie/original_name/text()", MovieNode),
                    [#xmlText{value=MovieDate}] = xmerl_xpath:string("/movie/released/text()", MovieNode),
                    [#xmlText{value=MovieOverview}] = xmerl_xpath:string("/movie/overview/text()", MovieNode),
                    [#xmlText{value=MovieUrl}] = xmerl_xpath:string("/movie/url/text()", MovieNode),
                    [#xmlText{value=MovieRating}] = xmerl_xpath:string("/movie/rating/text()", MovieNode),
                    AbsMovieRating = round(list_to_float(MovieRating)),
                    MovieRatingString = lists:flatten(lists:duplicate(AbsMovieRating, "*") ++ lists:duplicate(10 - AbsMovieRating, "-")),
                    MovieTrailerString = case xmerl_xpath:string("/movie/trailer", MovieNode) of
                        [#xmlText{value=MovieTrailer}] ->
                            io_lib:format("[Trailer:<~s>] ", MovieTrailer);
                        [] ->
                            ""
                    end,
                    Message = io_lib:format("~s (~s) [~s] ~s ~s[Info:<~s>]", [
                            MovieTitle,
                            MovieDate,
                            MovieRatingString,
                            MovieOverview,
                            MovieTrailerString,
                            MovieUrl]),
                    error_logger:info_msg("MESSAGE=~p~n", [Message]),
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
