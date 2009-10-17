all: check

.PHONY : clean

check: archicheck clean
	@ echo archicheck return code test
	@ test ! `./archicheck -I		>/dev/null 2>&1`
	@ test ! `./archicheck -I qsdqjh	>/dev/null 2>&1`
	@mkdir dir1
	@ ./archicheck -I dir1		>/dev/null 2>&1
	@ test ! `./archicheck -I dir2	>/dev/null 2>&1`
	@ test ! `./archicheck -I dir1 --list_components qdfgh	>/dev/null 2>&1`
	@ echo OK

clean:
	@ ${RM} -rf dir1
