CABAL=cabal --sandbox-config=../cabal.sandbox.config

all: build

debug: clean
	$(CABAL) configure --disable-library-profiling --enable-executable-dynamic --disable-optimization --ghc-options="-Wall"

clean:
	$(CABAL) clean

build:
	$(CABAL) build -j --only
	$(CABAL) haddock --hyperlink-sources
