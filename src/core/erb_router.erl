%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Route data records from the processor to the event handlers
%% -------------------------------------------------------------------
-module(erb_router).

%% API
-export([start_link/0, add_handler/1, notify/1]).

%% Server macro
-define(SERVER, ?MODULE).

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @spec Creates an event manager.
%% -------------------------------------------------------------------
start_link() ->
  gen_event:start_link({local, ?SERVER}). 

%% -------------------------------------------------------------------
%% @spec add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% @spec Adds an event handler
%% -------------------------------------------------------------------
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

%% -------------------------------------------------------------------
%% @spec notify(Event) -> ok | {error, Reason}
%% @spec Sends the Event through the event manager.
%% -------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?SERVER, Event).


%% ===================================================================
%% Internal functions
%% ===================================================================
