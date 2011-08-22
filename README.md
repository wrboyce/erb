# Erb

Erlang/OTP IRC Bot Framework

## Usage

compile the source (you'll need rebar):

    % make


create a release:

    % make rel


start the erb backend

    % rel/erb/bin/erb start


When running Erb for the first time, you'll need to do some configuration:

    % rel/erb/bin/erb console
    1> erb:add_server(network, {"irc.server.com", 6667}).
    ok
    2> erb:add_bot("BotNick", network, ["#channel"]).
    ok
