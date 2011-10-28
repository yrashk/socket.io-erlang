all: compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

test: force
	@./rebar eunit skip_deps=true

force:
	@true
