all: check doc

MD=03_dependency_list.md

.PHONY : clean

check: ../../obj/acc
	@ - ln -s ../../obj/acc .
	@ bbt -c --yes ${MD} -o results.md

clean:
	@ - ${RM} *.out acc

doc:
	@ cp ${MD} ../../docs/tests/${MD}
