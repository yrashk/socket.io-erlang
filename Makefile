all: compile

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

compile: deps
	@./rebar compile

test:
	@./rebar eunit skip_deps=true

dialyze: compile
	@./rebar dialyze skip_deps=true

clean:
	@./rebar clean
	@find . -name "erl_crash\.dump" | xargs rm -f

distclean: clean
	@rm -rf deps