#!/bin/sh

TESTDIR=$PWD/tests/
export TESTDIR

FILE1=highlight-indent-guides.el
TEST1=${TESTDIR}highlight-indent-guides-tests.el
TEST2=${TESTDIR}highlight-indent-guides-known-bugs-tests.el
echo "\$TEST1: $TEST1"

echo "\$TESTDIR: $TESTDIR"
#  echo \$EU27Q: $EU27Q

EMACS=emacs

echo "\$EMACS: $EMACS"

hig_tests () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
--eval "(setq python-indent-offset 4)" \
--eval "(setq python-indent-guess-indent-offset nil)" \
--eval "(setq python-indent-guess-indent-offset-verbose nil)" \
-load $FILE1 \
\
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

hig_tests
