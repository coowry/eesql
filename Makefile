compile:
	./rebar3 compile

dialyze:
	./rebar3 dialyzer -s

test:
	./rebar3 eunit

clean:
	./rebar3 clean

pull:
	git fetch --prune
	git checkout master && git merge origin/master
	git checkout develop && git merge origin/develop

push: pull
	git push --follow-tags origin master develop

.PHONY: compile test dialyze clean pull push


