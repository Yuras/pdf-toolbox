CABAL=cabal --sandbox-config=../cabal.sandbox.config

all: build

debug: clean
	$(CABAL) configure --disable-library-profiling --enable-executable-dynamic --disable-optimization --ghc-options="-Wall"

release: clean
	$(CABAL) configure --disable-library-profiling --disable-shared --ghc-options="-Wall"

clean:
	$(CABAL) clean

build:
	$(CABAL) build -j
