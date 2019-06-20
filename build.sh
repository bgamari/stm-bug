#!/usr/bin/env bash

set -ex

GHC=ghc
args="Main.hs -debug -O1 -ddump-simpl -fforce-recomp -dsuppress-coercions -Wall -ddump-to-file"

$GHC $args -dumpdir good -DGOOD
$GHC $args -dumpdir bad

