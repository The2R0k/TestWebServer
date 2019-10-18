compile:
	./rebar compile

get-deps:
	./rebar g-d

del-deps:
	rm -rf deps

run:
	erl -pa ebin deps/*/ebin -s test_web_server

build-and-run:
	./rebar g-d
	./rebar compile
	erl -pa ebin deps/*/ebin -s test_web_server

chmod-rebar:
	chmod u+x rebar

test:
	./rebar eunit
