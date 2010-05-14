# Erb

Erlang/OTP IRC Bot Framework

## Usage

compile the source:

    $ make


start an erlang shell:

    $ erl -pa ebin/


before running Erb for the first time, you will need to do a few things (replacing the correct stuff where appropriate):

    1> rr("src/core/erb_config_server.erl").
    2> mnesia:create_table(config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, config)}]).
    3> mnesia:dirty_write(#config{service=bot, config={"botnick", ["#chan1", "#chan2"]}}).
    4> mnesia:dirty_write(#config{service=server, config={"irc.freenode.net", 6667}).
    5> mnesia:dirty_write(#config{service=handlers, config=[erb_http_handler]}).


load and start the erb application:

    1> mnesia:start().
    2> inets:start(). %% required for erb_http_handler
    3> application:load(erb), application:start(erb).
