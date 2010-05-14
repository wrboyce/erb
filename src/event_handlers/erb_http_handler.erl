%% @title Erb "HTTP Title" Handler
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2010 Will Boyce
%% @doc Watches for links mentioned in channels and looks up the HTML Title
%% This handler works as a good example as a plugin, however it should also
%% be considered very experimental!
-module(erb_http_handler).
-author("Will Boyce").
-behaviour(gen_event).

%% Include files
-include("erb.hrl").

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {reqchanmap}).

%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Creates an event manager.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @doc Adds an event handler
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @spec init(Args) -> {ok, State}
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
init([]) ->
    {ok, #state{reqchanmap=ets:new(reqchanmap_ets, [ordered_set])}}.

%% @spec handle_event(data(), State) -> {ok, State}
%% @spec Event cast from erb_router when a line is received from the server.
handle_event(Data = #data{}, State) ->
    case Data#data.operation of
        privmsg ->
            lists:map(fun(Word) ->
                case string:to_lower(string:sub_string(Word, 1, 4)) of
                    "http" ->
                        {ok, RequestId} = httpc:request(get, {Word,[]}, [], [{sync, false}]),
                        error_logger:info_msg("Storing RequestId: ~p (~p)~n", [RequestId, {Word, Data#data.destination}]),
                        true = ets:insert(State#state.reqchanmap, {RequestId, {Word, Data#data.destination}});
                    _ ->
                        ok
                end
            end, string:tokens(Data#data.body, " "));
        _ ->
            ok
    end,
    {ok, State};

%% @spec handle_event(Event, State) -> {ok, State}                          |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% @doc Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
handle_event(_Event, State) ->
    {ok, State}.

%% @spec handle_call(Request, State) -> {ok, Reply, State}              |
%%                    {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                    {remove_handler, Reply}
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @spec handle_info({http, {RequestId, Response}}, State) -> {ok, State}
%% @doc Callback message received from an asyncronous httpc:request
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
            error_logger:warning_msg("Got unknown http response: ~p~n", [RequestId]),
            error
    end,
    {ok, State};

%% @spec handle_info(Info, State) -> {ok, State}                          |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                             remove_handler
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
handle_info(_Info, State) ->
    {ok, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
