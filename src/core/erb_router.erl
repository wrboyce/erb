%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Route data records from the processor to the event handlers
%% -------------------------------------------------------------------
-module(erb_router).

%% API
-export([start_link/0, add_handler/1]).

%% Server macro
-define(SERVER, ?MODULE).

%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @spec Creates an event manager.
%% -------------------------------------------------------------------
start_link() ->
    gen_event:start_link({global, ?SERVER}).

%% -------------------------------------------------------------------
%% @spec add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% @spec Adds an event handler
%% -------------------------------------------------------------------
add_handler(Module) ->
    gen_event:add_handler(?SERVER, Module, []).

%% ===================================================================
%% Internal functions
%% ===================================================================
