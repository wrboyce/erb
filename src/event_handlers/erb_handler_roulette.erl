%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Stateful Russian Roulette!
%% -------------------------------------------------------------------
-module(erb_handler_roulette).
-author("Will Boyce").
-behaviour(gen_event).
-include("erb.hrl").

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% Server macro
-define(SERVER, ?MODULE).

%% ===================================================================
%% gen_event callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Creates an event manager.
%% -------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% -------------------------------------------------------------------
%% @spec add_handler() -> ok | {'EXIT',Reason} | term()
%% @doc Adds an event handler
%% -------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%% ===================================================================
%% gen_event callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% -------------------------------------------------------------------
init([]) ->
    State = load_gun(),
    {ok, State}.

%% -------------------------------------------------------------------
%% @spec
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% @docWhenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%% -------------------------------------------------------------------
handle_event(Data, State) ->
    NewState = case Data#data.operation of
        privmsg ->
            case string:equal(Data#data.body, ":roulette") of
                true ->
                    [Head|Tail] = State,
                    case Head of
                        true ->
                            ok = gen_server:cast({global, erb_dispatcher}, {privmsg, Data#data.destination, "*bang*"}),
                            load_gun();
                        false ->
                            ok = gen_server:cast({global, erb_dispatcher}, {privmsg, Data#data.destination, "*click*"}),
                            Tail ++ [Head]
                    end;
                false ->
                    State
            end;
        _ ->
            State
    end,
    {ok, NewState}.

%% -------------------------------------------------------------------
%% @spec
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%% -------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% -------------------------------------------------------------------
%% @spec
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% -------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%% -------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @docWhenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
load_gun() ->
    shuffle([true, false, false, false, false, false]).

shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)),
   D1.
