#!/usr/bin/env make

TARGET = wordpuzzle
SRCS = $(wildcard *.hs */*.hs)

.PHONY: build
build:	check
	@stack build

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

.PHONY: exec
exec:
	@stack exec -- ${TARGET} -s 4 -m c -l adevcrsoi

.PHONY: dictionary
dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

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
