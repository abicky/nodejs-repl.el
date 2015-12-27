export EMACS = $(shell which emacs)
TARGET = nodejs-repl

all: $(TARGET).elc

clean:
	@cask clean-elc

test:
	cask exec ${EMACS} -Q --batch -L . -l test/test.el -f ert-run-tests-batch-and-exit

.el.elc:
	@cask build

.PHONY: clean test
