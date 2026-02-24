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

.PHONY:	bench
bench:
	@cabal bench

.PHONY:	exec
exec:
	cabal exec $(TARGET) -- $(ARGS)

.PHONY: dictionary
dictionary:
ifeq (,$(wildcard /usr/share/dict/british-english-huge))
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl -fS -s https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
else
	@echo using dictionary from /usr/share/dict/british-english-huge
	@cp /usr/share/dict/british-english-huge dictionary
endif
	@echo filtering dictionary ...
	@LC_ALL=C grep -E '^[a-z]{4,}$$' dictionary | sort -u > dictionary.tmp
	@mv dictionary.tmp dictionary

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
