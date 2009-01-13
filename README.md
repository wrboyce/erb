# Erb

Erlang/OTP IRC Bot Framework


## Usage

edit src/erb_config_server.erl, the settings should be rather self explinatory (along with the comments).

compile the source:
    $ make

start an erlang shell:
	$ erl -pa ebin/

load and start the erb application:
	1> application:load(erb), application:start(erb).
