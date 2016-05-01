#!/usr/bin/env bash

set -o errexit

cd tests

rm -rf test_results test_libs

mkdir test_libs
curl -s https://raw.githubusercontent.com/magnars/dash.el/master/dash.el > test_libs/dash.el

# unit tests
emacs -nw -Q --batch -l test_libs/dash.el\
      -l ../key-chord-multiple.el\
      -l key-chord-multiple-tests.el\
      --eval "(ert-run-tests-batch-and-exit)"

# acceptance tests
./do-acceptance-tests.exp
diff -urN test_expected test_results
