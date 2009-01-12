all:
	erlc -I include/ -o ebin/ src/*.erl

clean:
	rm ebin/*.beam
