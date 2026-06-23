#!/usr/bin/env make

.DEFAULT_GOAL	:= default

TARGET	:= wordpuzzle
CABAL	:= $(TARGET).cabal
SRCS	:= $(wildcard */*.hs)

.PHONY: default
default: format check build test ## Format check build test

.PHONY: help
help: ## Show this help message
	@echo ""
	@echo "Default goal: ${.DEFAULT_GOAL}"
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

.PHONY: all
all:	format check build test bench doc exec ## Format check build test bench doc exec

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
build:  $(SRCS) ## Build the project with stack
	@echo build ...
	@stack build --verbosity info --pedantic --no-test

.PHONY: test
test: ## Run test suite
	@echo test ...
	@stack test

.PHONY:	doc
doc: ## Generate Haddock documentation
	@echo doc ...
	@stack haddock --haddock-hyperlink-source

.PHONE: dependencies
dependencies: ## Regenerate project dependencies graph
	@echo generating dependencies graph ...
	@stack dot --external | dot -Tpng -o docs/dependencies.png

.PHONY:	copy
copy: doc ## Copy Haddock documentation to doc/html
	@echo copying documentation ...
	@DOC_ROOT=$$(stack path --local-doc-root) && \
	cp -r \
	  "$${DOC_ROOT}/wordpuzzle/wordpuzzle"/* \
	  doc/html/wordpuzzle/

.PHONY:	bench
bench: ## Run benchmarks
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY:	exec
exec: ## Execute the target with arguments
	@echo Run 9-letter puzzle with cadevrsoi ...
	@stack exec -- $(TARGET) -s 7 -m c -l cadevrsoi
	@echo Run Spelling Bee with cadevrsoi ...
	@stack exec -- $(TARGET) -s 7 -m c -l cadevrsoi -r

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
setup: ## Update stack and show paths and dependencies
	stack update
	stack path
	stack query
	stack ls dependencies

.PHONY:	ghci
ghci: ## Start GHCi with warnings disabled for type defaults
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY:	clean
clean: ## Clean build artifacts
	@stack clean

.PHONY:	cleanall
cleanall: clean ## Clean build artifacts and remove tags
	@stack purge
	@$(RM) tags
