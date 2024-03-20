#!/usr/bin/env make

.DEFAULT_GOAL	:= default

TARGET	:= wordpuzzle
SRCS	:= $(wildcard */*.hs)

ARGS	?= -s 7 -l cadevrsoi

.PHONY:	default
default: format check build test

.PHONY:	all
all:	format check build test bench doc exec

.PHONY:	format
format:
	@stylish-haskell --verbose --inplace $(SRCS)
	@cabal-fmt --inplace $(TARGET).cabal

.PHONY:	check
check:	tags lint

.PHONY:	tags
tags:
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY:	lint
lint:
	@hlint --color --show $(SRCS)
	@cabal check

.PHONY:	build
build:
	@stack build --verbosity info --pedantic --no-test

.PHONY:	test
test:
	@stack test

.PHONY:	exec
exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s

.PHONY:	bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY:	doc
doc:
	@stack haddock

.PHONY:	dictionary
dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

.PHONY:	setup
setup:
	stack update
	stack path
	stack query
	stack ls dependencies

.PHONY:	clean
clean:
	@stack clean
	@cabal clean

.PHONY:	cleanall
cleanall: clean
	@stack purge
	@rm -f stack.yaml.lock
	@rm -f tags
