#!/usr/bin/env make

.DEFAULT_GOAL	:= default

GHC_VERSION	:= 9.6.7
TARGET	:= wordpuzzle
CABAL	:= $(TARGET).cabal
SRCS	:= $(wildcard */*.hs)

ARGS	?= -s 7 -l cadevrsoi

.PHONY: default
default: format check build test ## Run the default pipeline

.PHONY: help
help: ## Show this help message
	@echo Available targets:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

.PHONY: all
all:	format check build test bench doc exec ## Run full pipeline including bench, docs, and exec

.PHONY: format
format:	$(SRCS) ## Format cabal file and Haskell sources
	@echo format ...
	@cabal-fmt --inplace $(CABAL)
	@stylish-haskell --inplace $(SRCS)

.PHONY: check
check:	tags lint ## Run static checks

.PHONY: tags
tags:	$(SRCS) ## Generate ctags for Haskell sources
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY: lint
lint:	$(SRCS) ## Run hlint and cabal check
	@echo lint ...
	@hlint --cross --color --show $(SRCS)
	@cabal check

.PHONY: build
build:  $(SRCS) ## Build project with cabal
	@echo build ...
	@cabal build

.PHONY: test
test: ## Run test suite
	@echo test ...
	@cabal test --test-show-details=direct

.PHONY:	doc
doc: ## Build Haddock documentation
	@echo doc ...
	@cabal haddock \
		--haddock-executables \
		--haddock-quickjump \
		--haddock-hyperlink-sources \
		lib:$(TARGET) \
		exe:$(TARGET)

.PHONY:	copy
copy: doc ## Copy generated Haddock output to doc/html
	@echo copying documentation ...
	@cp -r \
		dist-newstyle/build/x86_64-linux/ghc-$(GHC_VERSION)/$(TARGET)-1.0.1/x/$(TARGET)/doc/html/$(TARGET)/$(TARGET)/* \
		doc/html/$(TARGET)/

.PHONY:	bench
bench: ## Run benchmarks
	@cabal bench

.PHONY:	exec
exec: ## Run $(TARGET) with ARGS
	@cabal exec $(TARGET) -- $(ARGS)

.PHONY: dictionary
dictionary: ## Generate dictionary from /usr/share/dict/words
ifeq (,$(wildcard /usr/share/dict/words))
	@echo Warning: /usr/share/dict/words not found, skipping dictionary generation
else
	@echo filtering 4-letters or more words from dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' /usr/share/dict/words | sort -u > dictionary
	@echo $(shell wc -l < dictionary) words in dictionary
endif

.PHONY:	setup
setup: ## Init cabal config and update dependencies
ifeq (,$(wildcard ${CABAL_CONFIG}))
	-cabal user-config init
else
	@echo Using user-config from ${CABAL_CONFIG} ...
endif
	-cabal update --only-dependencies

.PHONY:	ghci
ghci: ## Open GHCi via cabal repl
	@cabal repl

.PHONY:	clean
clean: ## Clean build artifacts and tags
	-cabal clean
	-$(RM) tags
