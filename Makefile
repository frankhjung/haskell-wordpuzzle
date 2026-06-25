#!/usr/bin/env make

.DEFAULT_GOAL	:= default

GHC_VERSION	:= 9.6.7
TARGET	:= wordpuzzle
CABAL	:= $(TARGET).cabal
SRCS	:= $(wildcard */*.hs)

.PHONY: default
default: format check build test ## Run the default pipeline

.PHONY: all
all:	format check build test bench doc exec ## Run the full pipeline

.PHONY: help
help: ## Show this help message
	@echo ""
	@echo "Default goal: ${.DEFAULT_GOAL}"
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

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
doc: ## Generate Haddock documentation
	@echo doc ...
	@cabal haddock \
		--haddock-executables \
		--haddock-quickjump \
		--haddock-hyperlink-sources \
		lib:$(TARGET) \
		exe:$(TARGET)

.PHONY:	copy
copy: doc ## Copy Haddock documentation to doc/html
	@echo copying documentation ...
	@cp -r dist-newstyle/build/x86_64-linux/ghc-$(GHC_VERSION)/$(TARGET)-*/doc/html/ docs/

.PHONY:	bench
bench: ## Run benchmarks
	@cabal bench

.PHONY:	exec
exec: ## Run $(TARGET) with ARGS
	@echo Run 9-letter puzzle with cadevrsoi ...
	@cabal exec $(TARGET) -- -s 7 -m c -l cadevrsoi
	@echo Run Spelling Bee with cadevrsoi ...
	@cabal exec $(TARGET) -- -s 9 -m c -l cadevrsoi -r

.PHONY: dictionary
dictionary: ## Generate dictionary file from /usr/share/dict/words, filtering for 4+ letter words and excluding Roman numerals
ifeq (,$(wildcard /usr/share/dict/words))
	@echo Warning: /usr/share/dict/words not found, skipping dictionary generation
else
	@echo filtering 4-letters or more words from dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' /usr/share/dict/words \
	  | grep -Ev '^m{0,3}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$$' \
	  | sort -u > dictionary
	@echo $(shell wc -l < dictionary) words in dictionary
endif

.PHONY:	setup
setup: ## Initialize cabal config and update dependencies
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
	@cabal clean
	@$(RM) tags
