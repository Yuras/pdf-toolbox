
BUILD_CMD = ghc --make -outputdir=.build -Wall -i../core/lib -i../document/lib -i../content/lib -O -rtsopts # -prof -auto-all

all: change-document-title create-simple-pdf-file unpack-and-decrypt-pdf-file merge-pdf-files

clean:
	rm -rf .build
	rm change-document-title
	rm create-simple-pdf-file
	rm unpack-and-decrypt-pdf-file
	rm merge-pdf-files

change-document-title: change-document-title.hs
	$(BUILD_CMD) change-document-title.hs

create-simple-pdf-file: create-simple-pdf-file.hs
	$(BUILD_CMD) create-simple-pdf-file.hs

unpack-and-decrypt-pdf-file: unpack-and-decrypt-pdf-file.hs
	$(BUILD_CMD) unpack-and-decrypt-pdf-file.hs

merge-pdf-files: merge-pdf-files.hs
	$(BUILD_CMD) merge-pdf-files.hs
