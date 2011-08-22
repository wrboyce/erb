%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2011 Will Boyce
%% @doc Top level supervisor for each bot started
%% -------------------------------------------------------------------
-module(erb_bot_supervisor).
-author("Will Boyce").
-include("erb.hrl").


-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% -------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% -------------------------------------------------------------------
start_link(Bot) ->
    ProcName = list_to_atom("erb_bot_supervisor_" ++ atom_to_list(Bot#bot.id)),
    supervisor:start_link({local, ProcName}, ?MODULE, [Bot]).

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
init([Bot]) ->
        NewBot = Bot#bot{
            router = {global, list_to_atom("erb_router_" ++ atom_to_list(Bot#bot.id))},
            dispatcher = {global, list_to_atom("erb_dispatcher_" ++ atom_to_list(Bot#bot.id))}
        },
	CoreSupervisor	        = {erb_core_supervisor,
							{erb_core_supervisor, start_link, [NewBot]},
							permanent,
							infinity,
							supervisor,
							[erb_core_supervisor]},
	ModuleSupervisor	= {erb_module_supervisor,
							{erb_module_supervisor, start_link, [NewBot]},
							permanent,
							infinity,
							supervisor,
							[erb_module_supervisor, erb_module_manager]},
    {ok, {{one_for_all, 5, 60}, [CoreSupervisor, ModuleSupervisor]}}.

%% ===================================================================
%% Internal functions
%% ===================================================================
