#!/usr/bin/env make

TARGET	:= wordpuzzle
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -s 4 -m c -l adevcrsoi

.PHONY:	all bench clean cleanall dictionary doc exec ghci install setup style test

.PHONY: default
default:	check build test

.PHONY:	all
all:	check build test bench doc exec

.PHONY:	check
check:	tags style lint

.PHONY:	tags
tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY:	style
style:
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

.PHONY:	lint
lint:
	@echo lint ...
	@hlint --color $(SRCS)

.PHONY:	build
build:
	@echo build ...
	@stack build --pedantic --no-test

.PHONY:	test
test:
	@stack test

.PHONY:	exec
exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s

.PHONY:	bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY:	doc
doc:
	@stack haddock --no-rerun-tests --no-reconfigure

.PHONY:	install
install:
	@stack install --local-bin-path $(HOME)/bin
	-cp -pr .stack-work/benchmark.html doc/
	-cp -pr $(shell find .stack-work/install -type d -name hpc) doc/
	-cp -pr $(shell find .stack-work/dist -type d -name html) doc/

.PHONY:	dictionary
dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

.PHONY:	setup
setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

.PHONY:	ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY:	clean
clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix .hdevtools.sock

.PHONY:	cleanall
cleanall: clean
	@stack clean --full
	@$(RM) -rf .stack-work/ $(TARGET)
