# arch-tag: DTM builder

HAXML := HaXml-1.12/src

dtmconv: dtmconv.lhs
	ghc -cpp --make -O2 -i$(HAXML) -o dtmconv dtmconv.lhs 

dtmconv.static: dtmconv.lhs
	hmake -I$(HAXML) -nhc98 -o dtmconv.static dtmconv.lhs

clean:
	-for ASDF in hi o ; do find . -name "*.$$ASDF" -exec rm -rvf {} \; ; done
	-find . -name "*~" -exec rm -rvf {} \;
