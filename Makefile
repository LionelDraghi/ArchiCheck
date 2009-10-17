.SILENT:

all: check doc

build Obj/archicheck:
	@ gnat make -q -Parchicheck.gpr

check: Obj/archicheck
	$(MAKE) --directory=Tests

dashboard:
	sloccount Src OpenToken |grep "ada=" | \
          ploticus -prefab pie data=stdin labels=2 colors="blue red green orange" \
             explode=0.1 values=1 title="Ada sloc `date +%x`" \
             -png -o Doc/répartition_sloc.png

doc: dashboard
	/home/lionel/NaturalDocs-1.34/NaturalDocs \
		-i Doc \
		-i Src \
		-i Tests \
		-o FramedHTML Doc/Generated -p Doc/Natural_Docs

.PHONY : clean
clean:
	gnat clean -Parchicheck.gpr
	$(MAKE) --directory=Tests clean
