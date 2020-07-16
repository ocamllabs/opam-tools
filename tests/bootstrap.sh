#!/usr/bin/env bash

set -x

mkdir -p test-dir
cd test-dir

for p in patdiff mirage irmin; do
  mkdir -p $p
  cd $p
  opam exec -- opam tools -vv
  opam exec -- dune build @install @doc
  ls -la _opam/bin 
  cd ..
done
