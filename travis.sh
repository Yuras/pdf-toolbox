#! /bin/bash
set -e

cd core
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install --force-reinstall

cd ../content
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install --force-reinstall

cd ../document
cabal install --only-dependencies --enable-tests --enable-documentation
cabal configure --enable-tests --ghc-options="-Wall -Werror"
cabal build
cabal haddock
cabal test
cabal install --force-reinstall

cd ../examples
cabal install --only-dependencies
cabal configure --ghc-options="-Wall -Werror"
cabal build

# if [[ `ghc --numeric-version` == "7.8"* ]]; then
# cd ../viewer
# cabal install alex happy
# cabal install gtk2hs-buildtools
# cabal install --only-dependencies
# cabal configure --ghc-options="-Wall -Werror"
# cabal build
# fi
