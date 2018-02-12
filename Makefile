#!/usr/bin/env make

TARGET = wordpuzzle
SRCS = $(wildcard *.hs */*.hs)

.PHONY: all
all:	style lint build test bench doc

.PHONY: style
style:
	@stylish-haskell -c .stylish-haskell.yaml -i ${SRCS}

.PHONY: lint
lint:
	@hlint --color ${SRCS}

.PHONY: check
check:	lint style

.PHONY: build
build:	check
	@stack build

.PHONY: exec
exec:
	@stack exec ${TARGET}

.PHONY: test
test:
	@stack test

.PHONY: bench
bench:
	@stack bench

.PHONY: doc
doc:
	@stack haddock

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults
