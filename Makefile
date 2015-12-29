export EMACS ?= $(shell which emacs)
TARGET = nodejs-repl

all: compile

compile: $(TARGET).elc

clean:
	@cask clean-elc

test: compile
	cask exec ${EMACS} -Q --batch -L . -l test/test.el -f ert-run-tests-batch-and-exit
	${MAKE} clean

.el.elc:
	@cask build

.PHONY: clean test
