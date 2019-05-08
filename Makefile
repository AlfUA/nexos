# Variables
REBAR = rebar3

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile	

start:
	$(REBAR) compile
	erl -pa ./_build/default/lib/nexos/ebin -pa ./_build/default/lib/jsone/ebin

check:
	$(REBAR) eunit