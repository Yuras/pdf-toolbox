
SANDBOX=../cabal.sandbox.config
CABAL=cabal --require-sandbox --sandbox-config-file=$(SANDBOX)

all: build

debug:
	$(CABAL) configure --disable-library-profiling --disable-shared \
		--disable-optimization --enable-tests --enable-coverage \
		--ghc-options="-Wall"

clean:
	$(CABAL) clean

build:
	$(CABAL) build --only

test:
	$(CABAL) test --only

haddock:
	$(CABAL) haddock --hyperlink-sources

deps:
	$(CABAL) install --only-dependencies --enable-tests --ghc-options="-j4" #--dry-run

install:
	$(CABAL) install

.PHONY: all debug clean build test haddock deps
