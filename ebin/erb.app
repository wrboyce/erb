%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Erb Application Resource File
%% -------------------------------------------------------------------

{application, erb,
	[{description, "Erb IRC Bot."},
	{vsn, "SVN"},
	{modules, [
		erb_application,
		erb_supervisor,
		erb_config_server,
		erb_router,
		erb_event_supervisor,
		erb_event_monitor,
		erb_dispatcher,
		erb_processor,
		erb_connector
	]},
	{registered, [
		erb_application,
		erb_supervisor,
		erb_config_server,
		erb_router,
		erb_event_supervisor,
		erb_event_monitor,
		erb_dispatcher,
		erb_processor,
		erb_connector
	]},
	{applications, [kernel, stdlib]},
	{mod, {erb_application, []}}
]}.
