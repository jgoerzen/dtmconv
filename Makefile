# arch-tag: DTM builder

HAXML := HaXml-1.12/src

dtmconv: dtmconv.hs
	ghc -cpp --make -O2 -i$(HAXML) -o dtmconv dtmconv.hs 

dtmconv.static: dtmconv.hs
	hmake -I$(HAXML) -nhc98 -o dtmconv.static dtmconv.hs

clean:
	-for ASDF in hi o ; do find . -name "*.$$ASDF" -exec rm -rvf {} \; ; done
	-find . -name "*~" -exec rm -rvf {} \;
