%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Supervisor for the core processes
%% -------------------------------------------------------------------
-module(erb_core_supervisor).
-author("Will Boyce").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Server macro
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
	Router			= {erb_router,{
						erb_router, start_link, []},
						permanent,
						2000,
						worker,
						dynamic},
	Dispatcher		= {erb_dispatcher,
						{erb_dispatcher, start_link, []},
						permanent,
						2000,
						worker,
						[erb_dispatcher]},
	Processor		= {erb_processor,
						{erb_processor, start_link, []},
						permanent,
						2000,
						worker,
						[erb_router]},
	Connector		= {erb_connector,
						{erb_connector, start_link, []},
						permanent,
						2000,
						worker,
						[erb_connector]},

    {ok, {{one_for_all, 5, 60}, [Router, Dispatcher, Processor, Connector]}}.

%% ===================================================================
%% Internal functions
%% ===================================================================
