#!/usr/bin/env make

.PHONY:	default all bench clean check cleanall dictionary doc exec ghci install setup style tags test

TARGET	:= wordpuzzle
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -s 7 -l cadevrsoi

default: check build test

all:	check build test bench doc exec

check:	tags style lint

tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@echo style ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRCS)

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
	@stack haddock --no-haddock-deps

install: bench doc
	@stack install --local-bin-path $(HOME)/bin
	@cp -pr $(shell find .stack-work/dist -type d -name html) doc/
	@mv benchmark.html doc/

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
	@rm -f tags
	@rm -f $(wildcard *.hi **/*.hi)
	@rm -f $(wildcard *.o **/*.o)
	@rm -f $(wildcard *.prof **/*.prof)
	@rm -f $(wildcard *.tix **/*.tix)

cleanall: clean
	@stack clean --full
	@$(RM) -rf .stack-work/ $(TARGET)
