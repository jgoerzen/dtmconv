-- arch-tag: DTM builder

HAXML := HaXml-1.12/src

dtmconv: dtmconv.hs
	ghc -cpp --make -O2 -i$(HAXML) -o dtmconv dtmconv.hs 

clean:
	for ASDF in hi hs o \~; do find . -name "*$$ASDF" -exec rm -rvf {} \; ; done