all: 	deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

distclean: clean devclean relclean
	rebar delete-deps

test:
	rebar skip_deps=true eunit


## Release Targets
rel: deps
	cd rel
	rebar compile generate

relclean:
	rm -rf rel/erb
