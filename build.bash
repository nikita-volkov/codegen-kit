#!/bin/bash
set -eo pipefail

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./*.stack-work/*" \
    -not -path "./.git/*")
}

function build_and_test {
  stack build \
  --test \
  --fast \
  --ghc-options "-j6 +RTS -A128m -n2m -RTS -fwarn-incomplete-patterns"

}

function build {
  stack build \
  --fast
}

function demo {
  stack exec demo
}

format
build_and_test
demo
