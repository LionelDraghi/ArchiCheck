all: source_list

source_list: archicheck
	@ echo -I option tests
	@ test ! `archicheck -I		>/dev/null 2>&1`
	@ test ! `archicheck -I qsdqjh	>/dev/null 2>&1`
	@ archicheck -I dir1		>/dev/null 2>&1
	@ echo OK

	@ echo sources list tests
	@ touch dir1/a.ads
	@ touch dir1/a.adb
	@ touch dir1/b.ads
	@ > expected.1
	@ echo "${PWD}/dir1/a.ads"	>> expected.1
	@ echo "${PWD}/dir1/a.adb"	>> expected.1
	@ echo "${PWD}/dir1/b.ads"	>> expected.1
	@ archicheck -I dir1		>/dev/null 2>&1
	@ diff expected.1 file_list
	@ echo OK

.PHONY : clean
clean:
	${RM} *.output
