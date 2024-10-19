all: check doc

MD=02_source_list.md

.PHONY : clean

check: ../../obj/acc
	@ bbt -ad ${MD} -o results.md

clean:
	@ - ${RM} *.out

doc:
	@ cp ${MD} ../../docs/tests/${MD}

