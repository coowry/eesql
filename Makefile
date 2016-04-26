compile:
	./rebar3 compile

dialyze:
	./rebar3 dialyzer -s

test:
	./rebar3 eunit

clean:
	./rebar3 clean

.PHONY: compile test dialyze clean

