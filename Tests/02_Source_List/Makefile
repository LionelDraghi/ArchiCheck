all: check doc

MD=02_source_list.md

.PHONY : clean

check: ../../obj/acc
	@ - ln -s ../../obj/acc .
	@ bbt --cleanup ${MD} -o results.md

clean:
	@ - ${RM} *.out acc

doc:
	@ cp ${MD} ../../docs/tests/${MD}

