all: check doc

MD=01_command_line.md

.PHONY : clean

check: ../../obj/acc 
	@ bbt -ad ${MD} -o results.md

clean:
	@ - ${RM} *.out 

doc:
	@ cp ${MD} ../../docs/tests/${MD}
