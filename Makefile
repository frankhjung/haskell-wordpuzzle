#!/usr/bin/env make

.DEFAULT_GOAL	:= default
TARGET	:= wordpuzzle
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -s 7 -l cadevrsoi

.PHONY:	default
default: format check build test

.PHONY:	all
all:	format check build test bench doc exec

.PHONY:	format
format:
	@echo format ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRCS)
	@cabal-fmt --inplace wordpuzzle.cabal

.PHONY:	check
check:	tags lint

.PHONY:	tags
tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY:	lint
lint:
	@echo lint ...
	@hlint --color $(SRCS)
	@cabal check --verbose=3

.PHONY:	build
build:
	@echo build ...
	@stack build --verbosity info --pedantic --no-test

.PHONY:	test
test:
	@echo test ...
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

.PHONY:	ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY:	clean
clean:
	@stack clean
	@cabal clean

.PHONY:	cleanall
cleanall: clean
	@stack purge
	@rm -f tags
