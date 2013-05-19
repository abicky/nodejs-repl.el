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
	@sh test/test.sh test/test.el

.el.elc:
	emacs $(OPT) -batch -f batch-byte-compile $<

.PHONY: clean test
