#!/usr/bin/env bash

set -o errexit

cd tests

rm -rf test_results test_libs

mkdir test_libs
curl https://raw.githubusercontent.com/magnars/dash.el/master/dash.el > test_libs/dash.el

./do-acceptance-tests.exp
diff -urN test_expected test_results
