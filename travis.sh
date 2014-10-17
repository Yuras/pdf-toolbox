#! /bin/bash
set -e

cd core
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
