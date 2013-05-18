#!/bin/sh

EMACS="/usr/bin/env emacs"
OPTIONS="-L . -L $HOME/.emacs.d/site-lisp"
OUTPUT=$(mktemp el-expectations.XXXXXXXXXX)

ret=$($EMACS -q --no-site-file --batch $OPTIONS -l el-expectations -f batch-expectations $OUTPUT $1)
code=$?
cat $OUTPUT
rm $OUTPUT
exit $code
