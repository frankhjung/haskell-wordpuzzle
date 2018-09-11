#!/usr/bin/env make

TARGET	:= wordpuzzle
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -s 4 -m c -l adevcrsoi

.PHONY: default
default:	check build test

.PHONY: all
all:	check build test bench doc

.PHONY: check
check:	lint style tags

style:	$(SRCS)
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	@hlint --color $(SRCS)

tags:	$(SRCS)
	@hasktags --ctags --extendedctag $(SRCS)

build:	$(SRCS)
	@stack build

test:
	@stack test

bench:
	@stack bench

exec:
	stack exec -- $(TARGET) $(ARGS)

docs:
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin

.PHONY: dictionary
dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/
