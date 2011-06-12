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
-export([start_link/0, register/0, process/1]).

%% gen_fsm callbacks
-export([init/1, waiting/2, registering/2, ready/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Server macro
-define(SERVER, ?MODULE).

%% State record
-record(state, {nick, chans}).

%% ===================================================================
%% API
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> ok,Pid} | ignore | {error,Error}
%% @docCreates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%% -------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({global, ?SERVER}, ?MODULE, [], []).

%% -------------------------------------------------------------------
%% @spec register() -> ok.
%% @doc Perform the initial registation with the server
%% -------------------------------------------------------------------
register() ->
    gen_fsm:send_event(?SERVER, connected).

%% -------------------------------------------------------------------
%% @spec process(Data) -> ok.
%% @doc Handle lines from Connector and Route to Event Manager
%% -------------------------------------------------------------------
process(Line) ->
    gen_fsm:send_event(?SERVER, {recv, Line}),
    ok.

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
init([]) ->
    case gen_server:call({global, erb_config_server}, {getConfig, bot}) of
        {ok, {Nick, Chans}} ->
            {ok, waiting, #state{nick=Nick, chans=Chans}};
        noconfig ->
            error_logger:error_msg("[erb_processor] Unable to get configuration~n"),
            {stop, config_error}
    end.
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
    ok = gen_server:cast({global, erb_dispatcher}, {register, State#state.nick}),
    error_logger:info_msg("erb_processor switching state: waiting->registering~n"),
    {next_state, registering, State}.

registering({recv, {DateTime, Line}}, State) ->
    Data = parse_line(DateTime, Line),
    case Data#data.operation of
        ping ->
            ok = gen_server:cast({global, erb_dispatcher}, {pong, Data#data.body}),
            {next_state, registering, State};
        err_nicknameinuse ->
            ok = gen_server:cast({global, erb_dispatcher}, {nick, State#state.nick ++ "_"}),
            {next_state, registering, State#state{nick = State#state.nick ++ "_"}};
        rpl_welcome ->
            ok = gen_server:cast({global, erb_dispatcher}, {join, State#state.chans}),
            ok = gen_event:notify({global, erb_router}, Data),
            error_logger:info_msg("erb_processor switching state: registering->ready~n"),
            {next_state, ready, State};
        _ ->
            {next_state, registering, State}
    end.

ready({recv, {DateTime, Line}}, State) ->
    Data = parse_line(DateTime, Line),
    case Data#data.operation of
        ping ->
            ok = gen_server:cast({global, erb_dispatcher}, {pong, Data#data.body}),
            Result = {next_state, ready, State};
        nickchanged ->
            Result = {next_state, ready, State#state{nick = Data#data.body}};
        _ ->
            Result = {next_state, ready, State}
    end,
        gen_event:notify({global, erb_router}, Data),
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
parse_line(DateTime, [$: | Line]) ->
    BodyPos = string:chr(Line, $:),
    case BodyPos > 0 of
    true ->
        Header = string:substr(Line, 1, BodyPos - 1),
        Body = string:substr(Line, BodyPos + 1),
        HeaderBits = string:tokens(Header, " "),

        case length(HeaderBits) of
        1 ->
            #data{
                datetime = DateTime,
                origin = lists:nth(1, HeaderBits),
                body = Body
            };
        2 ->
            #data{
                datetime = DateTime,
                origin = lists:nth(1, HeaderBits),
                operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
                body = Body
            };
        _ ->
            #data{
                datetime = DateTime,
                origin = lists:nth(1, HeaderBits),
                operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
                destination = lists:nth(3, HeaderBits),
                options = lists:flatten(lists:nthtail(3, HeaderBits)),
                body = Body
            }
        end;
    false ->
        [Origin, Operation, Destination | _] = string:tokens(Line, " "),
        #data{
            datetime = DateTime,
            origin = Origin,
            operation = irc_lib:operation_to_atom(Operation),
            destination = Destination,
            body = ""
        }
    end;
%% -------------------------------------------------------------------
%% @private
%% @spec parse_line([$: | Line) -> #data
%% @doc Lines not starting with a colon are wrong, PING being the exception
%% -------------------------------------------------------------------
parse_line(DateTime, [$P, $I, $N, $G, $\s, $:|Server]) ->
#data{
    datetime = DateTime,
        operation=ping,
        body=Server
    };

parse_line(DateTime, Data) ->
    error_logger:warning_msg("Malformed Data from Server: ~p ~p~n", [DateTime, Data]),
    #data{operation=malformed, body=Data}.
