all: compile

compile:
	@./rebar compile

test:
	@./rebar eunit skip_deps=true
