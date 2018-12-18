# Erb
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fwrboyce%2Ferb.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fwrboyce%2Ferb?ref=badge_shield)


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


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fwrboyce%2Ferb.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fwrboyce%2Ferb?ref=badge_large)