
all: build

debug: clean
	cabal configure --disable-library-profiling --disable-shared --disable-optimization --ghc-options="-Wall"

release: clean
	cabal configure --disable-library-profiling --disable-shared --ghc-options="-Wall"

clean:
	cabal clean

build:
	cabal build
