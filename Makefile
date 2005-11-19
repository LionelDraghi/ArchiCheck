all: check

build Obj/archicheck:
	@ gnat make -q -Parchicheck.gpr

check: Obj/archicheck
	$(MAKE) --directory=Tests

.PHONY : clean
clean:
	gnat clean -Parchicheck.gpr
	$(MAKE) --directory=Tests clean
