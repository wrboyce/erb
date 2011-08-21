%% -------------------------------------------------------------------
%% @author Will Boyce <me@willboyce.com> [http://willboyce.com]
%% @copyright 2008-2011 Will Boyce
%% @doc Erb Application Resource File
%% -------------------------------------------------------------------

{application, erb,
    [{description, "Erb IRC Bot."},
    {vsn, "dev"},
    {modules, [
        irc_lib,
        erb_application,
        erb_supervisor,
        erb_config_server,
        erb_core_supervisor,
        erb_router,
        erb_dispatcher,
        erb_processor,
        erb_connector,
        erb_module_supervisor,
        erb_module_manager,
        erb_lastfm,
        erb_moviedb,
        erb_roulette,
        erb_seen,
        erb_tweets,
        erb_web_titles
    ]},
    {registered, [
        erb_config_server,
        erb_connector,
        erb_processor,
        erb_dispatcher,
        erb_router,
        erb_module_supervisor,
        erb_module_manager
    ]},
    {applications, [kernel, stdlib, sasl, mnesia, inets]},
    {mod, {erb_application, []}}
]}.
