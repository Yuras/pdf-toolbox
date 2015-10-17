#! /bin/bash
set -e

cd core
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install

cd ../content
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install

cd ../document
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install

cd ../examples
cabal install --only-dependencies
cabal configure --ghc-options="-Wall -Werror"
cabal build
