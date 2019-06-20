#!/usr/bin/env bash

set -ex

GHC=ghc
args="$args Main.hs \
  -debug -O1 -fforce-recomp -Wall \
  -ddump-simpl -dsuppress-coercions -ddump-to-file -dsuppress-uniques \
  -dcore-lint -dstg-lint -dcmm-lint -dppr-cols=120"

$GHC $args -o Main-good -dumpdir good -DGOOD
$GHC $args -o Main-bad -dumpdir bad

./Main-good 2>&1 | grep -q 'Prelude.undefined' || ( echo "good failed"; exit 1 )
./Main-bad && ( echo "bad worked"; exit 1 )

