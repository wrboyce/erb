all:
	find src/ -name \*.erl -exec erlc -I include/ -o ebin/ {} \+

clean:
	rm ebin/*.beam
