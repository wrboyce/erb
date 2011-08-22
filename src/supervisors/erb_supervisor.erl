%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Top level supervisor for Erb
%% -------------------------------------------------------------------
-module(erb_supervisor).
-author("Will Boyce").
-include("erb.hrl").

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
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

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
	ConfigServer		= {erb_config_server,
							{erb_config_server, start_link, []},
							permanent,
							2000,
							worker,
							[erb_config_server]},
	BotManager		= {erb_bot_manager,
							{erb_bot_manager, start_link, []},
							permanent,
							2000,
							worker,
							[erb_bot_manager]},
    {ok, {{one_for_all, 5, 60}, [ConfigServer, BotManager]}}.

%% ===================================================================
%% Internal functions
%% ===================================================================
