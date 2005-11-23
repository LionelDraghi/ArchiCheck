all: check

.PHONY : clean

check: archicheck
	@ echo archicheck return code test
	@ test ! `archicheck -I		>/dev/null 2>&1`
	@ test ! `archicheck -I qsdqjh	>/dev/null 2>&1`
	@ archicheck -I dir1		>/dev/null 2>&1
	@ test ! `archicheck -I dir2	>/dev/null 2>&1`
	@ test ! `archicheck -I dir1 --list_components qdfgh	>/dev/null 2>&1`
	@ echo OK

clean:

