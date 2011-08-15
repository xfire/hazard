#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG=testsuite
fi

SUITE="./dist/build/testsuite/testsuite"
rm -f testsuite.tix

cabal build && $SUITE -j1

case $1 in
    "-c" | "--coverage")
        HPC_DIR="./dist/coverage"

        rm -Rf "$hPC_DIR"
        mkdir -p "$HPC_DIR"
        hpc markup --destdir="$HPC_DIR" testsuite >/dev/null 2>&1

        echo -n "\n\nCoverage generated in $HPC_DIR\n"
    ;;
esac

