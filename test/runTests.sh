#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG=testsuite
fi

SUITE=./dist/build/testsuite/testsuite

cabal build && $SUITE -j1 $*
