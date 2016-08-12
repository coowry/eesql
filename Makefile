default: compile

help:
	@echo Usage: make [compile] [test] [dialyze] [clean] [pull] [push]

all: compile dialyze test

compile:
	./rebar3 compile

dialyze:
	./rebar3 dialyzer

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

.PHONY: default help compile all test dialyze clean pull push
