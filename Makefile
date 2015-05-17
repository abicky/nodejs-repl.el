TARGET=nodejs-mode
ifdef LIB
	OPT=-L $(LIB)
else
	OPT=-L $(HOME)/.emacs.d/site-lisp
endif

all: $(TARGET).elc

clean:
	@rm -f $(TARGET).elc $(TARGET).el~

test:
	cask exec /usr/bin/env emacs -Q --batch -L . -l test/test.el -f ert-run-tests-batch-and-exit

.el.elc:
	emacs $(OPT) -batch -f batch-byte-compile $<

.PHONY: clean test
