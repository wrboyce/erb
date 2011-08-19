%% -------------------------------------------------------------------
%% @author Will Boyce <mail@willboyce.com> [http://willboyce.com]
%% @copyright 2008 Will Boyce
%% @doc Erb Application Resource File
%% -------------------------------------------------------------------

{application, erb,
	[{description, "Erb IRC Bot."},
	{vsn, "dev"},
	{modules, [
		erb_application,
		erb_supervisor,
		erb_config_server,
		erb_module_supervisor,
		erb_router,
		erb_dispatcher,
		erb_processor,
		erb_connector
	]},
	{registered, [
		erb_application,
		erb_supervisor,
		erb_config_server,
		erb_module_supervisor,
		erb_router,
		erb_dispatcher,
		erb_processor,
		erb_connector
	]},
	{applications, [kernel, stdlib]},
	{mod, {erb_application, []}}
]}.
