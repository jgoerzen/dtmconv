# arch-tag: DTM builder

HAXML := HaXml-1.12/src

dtmconv: dtmconv.hs
	ghc -cpp --make -O2 -i$(HAXML) -o dtmconv dtmconv.hs 
	strip dtmconv

dtmconv.static: dtmconv.hs
	ghc -optc-static -optl-static -static -fvia-C -cpp -static --make -O2 -i$(HAXML) -o dtmconv.static dtmconv.hs
	strip dtmconv.static

dtmconv.nhc98: dtmconv.hs
	hmake -I$(HAXML) -nhc98 -o dtmconv.nhc98 dtmconv.hs

hugs:
	hugs "-Fcpp -P -traditional -D__HUGS__" -98 +o -P$(HAXML): dtmconv.hs
clean:
	-for ASDF in hi o ; do find . -name "*.$$ASDF" -exec rm -rvf {} \; ; done
	-find . -name "*~" -exec rm -rvf {} \;
