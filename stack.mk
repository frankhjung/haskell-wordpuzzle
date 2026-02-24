#!/usr/bin/env make

.DEFAULT_GOAL	:= default

TARGET	:= wordpuzzle
CABAL	:= $(TARGET).cabal
SRCS	:= $(wildcard */*.hs)

ARGS	?= -s 7 -l cadevrsoi

.PHONY: default
default: format check build test

.PHONY: all
all:	format check build test bench doc exec

.PHONY: format
format:	$(SRCS)
	@echo format ...
	@cabal-fmt --inplace $(CABAL)
	@stylish-haskell --inplace $(SRCS)

.PHONY: check
check:	tags lint

.PHONY: tags
tags:	$(SRCS)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY: lint
lint:	$(SRCS)
	@echo lint ...
	@hlint --cross --color --show $(SRCS)
	@cabal check

.PHONY: build
build:  $(SRCS)
	@echo build ...
	@stack build --verbosity info --pedantic --no-test

.PHONY: test
test:
	@echo test ...
	@stack test

.PHONY:	doc
doc:
	@echo doc ...
	@stack haddock --haddock-hyperlink-source

.PHONY:	bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY:	exec
exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s

.PHONY: dictionary
dictionary:
ifeq (,$(wildcard /usr/share/dict/british-english-huge))
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl -s https://raw.githubusercontent.com/dwyl/english-words/master/words.txt \
		| LC_ALL=C sort -u > dictionary
else
	@echo using dictionary from /usr/share/dict/british-english-huge
	@cp /usr/share/dict/british-english-huge dictionary
endif
	@echo filtering dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' dictionary > dictionary.tmp
	@mv dictionary.tmp dictionary

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

.PHONY:	distclean
distclean: clean
	@stack purge
	@$(RM) dictionary
	@$(RM) tags
