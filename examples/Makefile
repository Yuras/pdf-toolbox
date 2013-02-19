
BUILD_CMD = ghc --make -outputdir=.build -Wall -i../core/lib -i../document/lib -i../content/lib

all: change-document-title create-simple-pdf-file

clean:
	rm -rf .build
	rm change-document-title
	rm create-simple-pdf-file

change-document-title: change-document-title.hs
	$(BUILD_CMD) change-document-title.hs

create-simple-pdf-file: create-simple-pdf-file.hs
	$(BUILD_CMD) create-simple-pdf-file.hs
