#!/usr/bin/env make

.PHONY:	default all bench clean check cleanall dictionary doc exec format ghci setup tags test

TARGET	:= wordpuzzle
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -s 7 -l cadevrsoi

default: format check build test

all:	format check build test bench doc exec

format:
	@echo format ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRCS)
	@cabal-fmt --inplace wordpuzzle.cabal

check:	tags lint

tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

lint:
	@echo lint ...
	@hlint --color $(SRCS)
	@cabal check --verbose=3

build:
	@echo build ...
	@stack build --verbosity info --pedantic --no-test

test:
	@echo test ...
	@stack test

exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

doc:
	@stack haddock

dictionary:
ifneq ("$(wildcard /usr/share/dict/british-english-huge)","")
	@echo using dictionary from /usr/share/dict/british-english-huge
	@ln -sf /usr/share/dict/british-english-huge dictionary
else
	@echo using dictionary from https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
	@curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt -o dictionary
endif

setup:
	stack update
	stack path
	stack query
	stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@cabal clean

cleanall: clean
	@stack purge
	@rm -f tags
