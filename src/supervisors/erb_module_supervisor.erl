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
    ModuleSpecs = case gen_server:call({global, erb_config_server}, {getConfig, Bot#bot.id, modules}) of
        {ok, Modules} ->
            lists:map(fun(M) ->
                erb_module_manager:gen_spec(M, Bot)
            end, Modules);
        noconfig ->
            []
    end,
    ModuleManager = erb_module_manager:gen_spec(erb_module_manager, Bot),
    ChildSpecs = [ModuleManager|ModuleSpecs],
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% ===================================================================
%% Internal functions
%% ===================================================================
