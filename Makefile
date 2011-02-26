all: compile

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

compile:
	@./rebar compile

test: force
	@./rebar eunit skip_deps=true

force: 
	@true
