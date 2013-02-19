
BUILD_CMD = ghc --make -outputdir=.build -Wall -i../core/lib -i../document/lib -i../content/lib

all: change-document-title

clean:
	rm -rf .build
	rm change-document-title

change-document-title: change-document-title.hs
	$(BUILD_CMD) change-document-title.hs
