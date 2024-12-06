all: check doc

MD=01_command_line.md

.PHONY : clean

check: ../../obj/acc 
	@ - ln -s ../../obj/acc .
	@ bbt -c --yes ${MD} -o results.md

clean:
	@ - ${RM} *.out acc gmon.out template.ac 

doc:
	@ cp ${MD} ../../docs/tests/${MD}
