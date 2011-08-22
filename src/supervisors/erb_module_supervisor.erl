%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Supervisor for Modules
%% -------------------------------------------------------------------
-module(erb_module_supervisor).
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
    ProcName = list_to_atom("erb_module_supervisor_" ++ atom_to_list(Bot#bot.id)),
    supervisor:start_link({global, ProcName}, ?MODULE, [Bot]).

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
    case gen_server:call({global, erb_config_server}, {getConfig, Bot, modules}) of
        {ok, Modules} ->
            ChildSpecs = lists:map(fun(M) ->
                {M, {M, start_link, [Bot]}, permanent, 2000, worker, [M]}
            end, [erb_module_manager] ++ Modules),
            {ok, {{one_for_one, 5, 60}, ChildSpecs}};
        noconfig ->
            ChildSpecs = [{erb_module_manager, {erb_module_manager, start_link, [Bot]}, permanent, 2000, worker, [erb_module_manager]}],
            {ok, {{one_for_one, 5, 60}, ChildSpecs}}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
