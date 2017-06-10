.SILENT:

all: check 

build Obj/archicheck:
	gnat make -q -Parchicheck.gpr

check: Obj/archicheck
	$(MAKE) --directory=Tests

dashboard:
	sloccount Src Tests |grep "ada=" |  ploticus  -prefab pie 		\
		data=stdin labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"		\
		-png -o Doc/sloc.png

doc: dashboard
	naturaldocs -i Doc -i Src -i Tests 						\
		-s Default archicheck							\
		-xi _darcs										\
		-xi Obj										\
		-o FramedHTML Doc/Generated -p Doc/Natural_Docs
	cp -p Doc/Archicheck_Overview.pdf Doc/Generated

.PHONY : clean
clean:
	gnat clean -q -Parchicheck.gpr
	$(MAKE) --directory=Tests clean
