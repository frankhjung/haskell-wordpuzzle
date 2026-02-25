#!/usr/bin/env make

.DEFAULT_GOAL	:= default

TARGET	:= wordpuzzle
CABAL	:= $(TARGET).cabal
SRCS	:= $(wildcard */*.hs)

ARGS	?= -s 7 -l cadevrsoi

.PHONY: default
default: format check build test

.PHONY: help
help:
	@echo "Targets:"
	@echo "  default    format check build test"
	@echo "  all        format check build test bench doc exec"
	@echo "  format     format cabal file and sources"
	@echo "  check      tags lint"
	@echo "  tags       generate ctags"
	@echo "  lint       run hlint and cabal check"
	@echo "  build      stack build"
	@echo "  test       stack test"
	@echo "  doc        stack haddock"
	@echo "  copy       copy haddock output to doc/html"
	@echo "  bench      stack bench"
	@echo "  exec       run $(TARGET) with ARGS"
	@echo "  dictionary generate dictionary from /usr/share/dict/words"
	@echo "  setup      stack update/path/query/deps"
	@echo "  ghci      stack ghci"
	@echo "  clean      stack clean"
	@echo "  cleanall   stack purge and remove tags"

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

.PHONY:	copy
copy: doc
	@echo copying documentation ...
	@DOC_ROOT=$$(stack path --local-doc-root) && \
	cp -r \
	  "$${DOC_ROOT}/wordpuzzle/wordpuzzle"/* \
	  doc/html/wordpuzzle/

.PHONY:	bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY:	exec
exec:
	@stack exec -- $(TARGET) $(ARGS)

.PHONY: dictionary
dictionary:
ifeq (,$(wildcard /usr/share/dict/words))
	@echo Warning: /usr/share/dict/words not found, skipping dictionary generation
else
	@echo filtering 4-letters or more words from dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' /usr/share/dict/words | sort -u > dictionary
	@echo $(shell wc -l < dictionary) words in dictionary
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

.PHONY:	cleanall
cleanall: clean
	@stack purge
	@$(RM) tags
