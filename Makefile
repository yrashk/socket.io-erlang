all: compile

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

compile:
	@./rebar compile

test:
	@./rebar eunit skip_deps=true
