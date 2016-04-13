default:
	@echo Usage: make [pull] [push]

pull:
	git checkout develop
	git pull
	git checkout master
	git pull

push: pull
	git push origin master develop
	git push --tags

.PHONY: default pull push
