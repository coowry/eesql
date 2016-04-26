compile:
	./rebar3 compile

dialyze:
	./rebar3 dialyzer -s

test:
	./rebar3 eunit

.PHONY: compile test dialyze

