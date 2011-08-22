%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Receive messages from connector, process into data record and
%%        notify the router.
%% -------------------------------------------------------------------
-module(erb_processor).
-author("Will Boyce").
-behaviour(gen_fsm).
-include("erb.hrl").

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, waiting/2, registering/2, ready/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {bot, nick2account}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> ok,Pid} | ignore | {error,Error}
%% @docCreates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%% -------------------------------------------------------------------
start_link(Bot, Id) ->
    error_logger:info_msg("make way for the processor"),
    gen_fsm:start_link(Id, ?MODULE, [Bot], []).

%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% @docWhenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%% -------------------------------------------------------------------
init([Bot]) ->
    State = #state{bot=Bot, nick2account=ets:new(processor_nick2account, [private])},
    {ok, waiting, State}.

%% -------------------------------------------------------------------
%% @spec
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% @docThere should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%% -------------------------------------------------------------------
waiting(connected, State) ->
    ok = gen_server:cast((State#state.bot)#bot.dispatcher, {register, (State#state.bot)#bot.nick}),
    error_logger:info_msg("erb_processor switching state: waiting->registering~n"),
    {next_state, registering, State};
waiting(Request, State) ->
    {next_state, waiting, State}.

registering({recv, {DateTime, Line}}, State) ->
    Data = parse_line(State#state.bot, DateTime, Line),
    case Data#data.operation of
        ping ->
            ok = gen_server:cast((State#state.bot)#bot.dispatcher, {pong, Data#data.body}),
            {next_state, registering, State};
        err_nicknameinuse ->
            NewNick = (State#state.bot)#bot.nick ++ "_",
            NewBot = (State#state.bot)#bot{ nick=NewNick },
            ok = gen_server:cast((State#state.bot)#bot.dispatcher, {nick, NewNick}),
            {next_state, registering, State#state{ bot=NewBot }};
        rpl_welcome ->
            Chans = (State#state.bot)#bot.chans,
            ok = gen_server:cast((State#state.bot)#bot.dispatcher, {join, Chans}),
            ok = gen_server:cast((State#state.bot)#bot.router, {data, State#state.bot, Data}),
            error_logger:info_msg("erb_processor switching state: registering->ready~n"),
            {next_state, ready, State};
        _ ->
            {next_state, registering, State}
    end.

ready({recv, {DateTime, Line}}, State) ->
    Data = parse_line(State#state.bot, DateTime, Line),
    Result = case Data#data.operation of
        ping ->
            ok = gen_server:cast((State#state.bot)#bot.dispatcher, {pong, Data#data.body}),
            {next_state, ready, State};
        nickchanged ->
            NewBotState = (State#state.bot)#bot{ nick = Data#data.body },
            {next_state, ready, State#state{bot = NewBotState }};
        join ->
            case string:tokens(Data#data.origin, "!@") of
                [Nick,_User,_Host] ->
                    gen_server:cast((State#state.bot)#bot.dispatcher, {whois, Nick});
                _ ->
                    pass
            end,
            {next_state, ready, State};
        rpl_whoisaccount ->
            [Nick,Account] = Data#data.options,
            true = ets:insert(State#state.nick2account, {Nick, Account}),
            {next_state, ready, State};
        part ->
            case string:tokens(Data#data.origin, "!@") of
                [Nick,_User,_Host] ->
                    true = ets:delete(State#state.nick2account, Nick);
                _ ->
                    pass
            end,
            {next_state, ready, State};
        _ ->
            {next_state, ready, State}
    end,
    AugmentedData = case Data#data.origin of
        undefined ->
            Data;
        _ -> case string:tokens(Data#data.origin, "!@") of
            [N,_,_] ->
                case ets:lookup(State#state.nick2account, N) of
                    [{_,A}] ->
                        Data#data{ account = A };
                    _ ->
                        Data
                end;
            _ ->
                Data
        end
    end,
    gen_server:cast((State#state.bot)#bot.router, {data, AugmentedData}),
    Result.

%% -------------------------------------------------------------------
%% @spec
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% @doc There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @spec
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                          NextState} |
%%                                          {next_state, NextStateName,
%%                              NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%% -------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% -------------------------------------------------------------------
%% @spec
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%% -------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% -------------------------------------------------------------------
%% @spec
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% @doc This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%% -------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% -------------------------------------------------------------------
%% @spec terminate(Reason, StateName, State) -> void()
%% @docThis function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%% -------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%% -------------------------------------------------------------------
%% @spec code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% @doc Convert process state when code is changed
%% -------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
%% -------------------------------------------------------------------
%% @private
%% @spec parse_line([$: | Line) -> #data
%% @doc Parse RAW data into a #data record.
%% -------------------------------------------------------------------
parse_line(Bot, DateTime, [$: | Line]) ->
    BodyPos = string:chr(Line, $:),
    Data = case BodyPos > 0 of
        true ->
            Header = string:substr(Line, 1, BodyPos - 1),
            Body = string:substr(Line, BodyPos + 1),
            HeaderBits = string:tokens(Header, " "),

            case length(HeaderBits) of
                1 ->
                    #data{
                        bot = Bot,
                        datetime = DateTime,
                        origin = lists:nth(1, HeaderBits),
                        body = Body
                    };
                2 ->
                    #data{
                        bot = Bot,
                        datetime = DateTime,
                        origin = lists:nth(1, HeaderBits),
                        operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
                        body = Body
                    };
                _ ->
                    #data{
                        bot = Bot,
                        datetime = DateTime,
                        origin = lists:nth(1, HeaderBits),
                        operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
                        destination = lists:nth(3, HeaderBits),
                        options = lists:nthtail(3, HeaderBits),
                        body = Body
                    }
            end;
        false ->
            [Origin, Operation, Destination | _] = string:tokens(Line, " "),
            #data{
                bot = Bot,
                datetime = DateTime,
                origin = Origin,
                operation = irc_lib:operation_to_atom(Operation),
                destination = Destination,
                body = ""
            }
    end,
    case {Data#data.operation, Data#data.body} of
        {privmsg, ["."|_]} ->
            Data#data{
                bot = Bot,
                datetime = DateTime,
                operation = command,
                body = string:tokens(Data#data.body, " ")
            };
        _ ->
            Data
    end;
%% -------------------------------------------------------------------
%% @private
%% @spec parse_line([$: | Line) -> #data
%% @doc Lines not starting with a colon are wrong, PING being the exception
%% -------------------------------------------------------------------
parse_line(Bot, DateTime, [$P, $I, $N, $G, $\s, $:|Server]) ->
#data{
        bot = Bot,
        datetime = DateTime,
        operation=ping,
        body=Server
    };

parse_line(_Bot, DateTime, Data) ->
    error_logger:warning_msg("Malformed Data from Server: ~p ~p~n", [DateTime, Data]),
    #data{operation=malformed, body=Data}.
