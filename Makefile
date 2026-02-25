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
	@echo "  build      cabal build"
	@echo "  test       cabal test"
	@echo "  doc        cabal haddock"
	@echo "  copy       copy haddock output to doc/html"
	@echo "  bench      cabal bench"
	@echo "  exec       run $(TARGET) with ARGS"
	@echo "  dictionary generate dictionary from /usr/share/dict/words"
	@echo "  setup      init cabal config and update deps"
	@echo "  clean      cabal clean"
	@echo "  cleanall   clean and remove tags"

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
	@cabal build

.PHONY: test
test:
	@echo test ...
	@cabal test --test-show-details=direct

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock \
		--haddock-executables \
		--haddock-quickjump \
		--haddock-hyperlink-sources \
		lib:$(TARGET) \
		exe:$(TARGET)

.PHONY: copy
copy: doc
	@echo copying documentation ...
	@cp -r \
		dist-newstyle/build/x86_64-linux/ghc-9.6.7/wordpuzzle-1.0.0/x/wordpuzzle/doc/html/wordpuzzle/wordpuzzle/* \
		doc/html/wordpuzzle/

.PHONY:	bench
bench:
	@cabal bench

.PHONY:	exec
exec:
	cabal exec $(TARGET) -- $(ARGS)

.PHONY: dictionary
dictionary:
ifeq (,$(wildcard /usr/share/dict/words))
	@echo Warning: /usr/share/dict/words not found, skipping dictionary generation
else
	@echo filtering 4-letters or more words from dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' /usr/share/dict/words | sort -u > dictionary
	@echo $(shell wc -l < dictionary) words in dictionary
endif

.PHONY: setup
setup:
ifeq (,$(wildcard ${CABAL_CONFIG}))
	-cabal user-config init
else
	@echo Using user-config from ${CABAL_CONFIG} ...
endif
	-cabal update --only-dependencies

.PHONY: clean
clean:
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@$(RM) tags
