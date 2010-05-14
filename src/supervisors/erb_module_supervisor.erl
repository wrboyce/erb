%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Supervisor for Modules
%% -------------------------------------------------------------------
-module(erb_module_supervisor).
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
    case gen_server:call({global, erb_config_server}, {getConfig, modules}) of
        {ok, Modules} ->
            ChildSpecs = lists:map(fun(M) ->
                {M, {M, start_link, []}, permanent, 2000, worker, [M]}
            end, Modules),
            {ok, {{one_for_one, 5, 60}, ChildSpecs}};
        noconfig ->
            {ok, {{one_for_one, 5, 60}, []}}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
