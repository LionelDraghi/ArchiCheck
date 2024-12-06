all: check doc

.PHONY : clean

MD=04_component_list.md

check: ../../obj/acc
	@ - ln -s ../../obj/acc .
	@ bbt -c --yes ${MD} -o results.md

clean:
	@ - ${RM} component_list.? *.out acc gmon.out template.ac

doc:
	@ cp ${MD} ../../docs/tests/${MD}
