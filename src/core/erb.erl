-module(erb).
-export([add_server/2, add_bot/3]).

add_server(Network, {Host, Port}) ->
    ok = gen_server:cast({global, erb_config_server}, {addServer, Network, {Host, Port}}).

add_bot(Nick, Network, Chans) ->
    ok = gen_server:cast({global, erb_bot_manager}, {addBot, Nick, Network, Chans}).
