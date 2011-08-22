-module(erb).
-export([add_server/2, add_bot/3, add_module/2, add_admin/2]).

add_server(Network, {Host, Port}) ->
    ok = gen_server:cast({global, erb_config_server}, {addServer, Network, {Host, Port}}).

add_bot(Nick, Network, Chans) ->
    ok = gen_server:cast({global, erb_bot_manager}, {addBot, Nick, Network, Chans}).

add_module(BotId, Module) ->
    Modules = case gen_server:call({global, erb_config_server}, {getConfig, BotId, modules}) of
        {ok, Config} -> Config;
        noconfig -> []
    end,
    case lists:member(Module, Modules) of
        false ->
            NewModules = [Module|Modules],
            ok = gen_server:cast({global, erb_config_server}, {putConfig, BotId, modules, NewModules});
        _ ->
            pass
    end.

add_admin(BotId, Admin) ->
    Admins = case gen_server:call({global, erb_config_server}, {getConfig, BotId, admins}) of
        {ok, Config} -> Config;
        noconfig -> []
    end,
    case lists:member(Admin, Admins) of
        false ->
            NewAdmins = [Admin|Admins],
            ok = gen_server:cast({global, erb_config_server}, {putConfig, BotId, admins, NewAdmins});
        _ ->
            pass
    end.
