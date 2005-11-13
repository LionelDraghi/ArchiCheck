all: source_list

source_list: archicheck
	@ echo -I option tests
	@ test ! `archicheck -I		>/dev/null 2>&1`
	@ test ! `archicheck -I qsdqjh	>/dev/null 2>&1`
	@ archicheck -I dir1		>/dev/null 2>&1
	@ echo OK

.PHONY : clean
clean:
	${RM} *.output
