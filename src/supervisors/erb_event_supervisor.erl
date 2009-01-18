%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Supervisor for Event Handler Monitors
%% -------------------------------------------------------------------
-module(erb_event_supervisor).
-author("Will Boyce").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

% Server macro
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% -------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% -------------------------------------------------------------------
init([]) ->
	case erb_config_server:get_config(?SERVER) of
		{ok, Handlers} ->
			ChildSpecs = lists:map(fun(H) ->
				{H, {erb_event_monitor, start_link, [H]}, permanent, 2000, worker, [H]}
			end, Handlers),
			{ok, {{one_for_one, 1, 10}, ChildSpecs}};
		noconfig ->
			{error, config_error}
	end.

%% ===================================================================
%% Internal functions
%% ===================================================================
