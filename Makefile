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
check:	tags style lint

tags:	$(SRCS)
	@hasktags --ctags --extendedctag $(SRCS)

style:	$(SRCS)
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	@hlint --color $(SRCS)

build:	$(SRCS)
	@stack build

.PHONY: test
test:
	@stack test --coverage

.PHONY: bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY: exec
exec:
	stack exec -- $(TARGET) $(ARGS) +RTS -s

.PHONY: doc
doc:
	@stack haddock

.PHONY: install
install:
	@stack install --local-bin-path $(HOME)/bin
	-cp -pr .stack-work/benchmark.html doc/
	-cp -pr $(shell find .stack-work/install -type d -name hpc) doc/
	-cp -pr $(shell find .stack-work/dist -type d -name html) doc/

.PHONY: dictionary
dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

.PHONY: setup
setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/
