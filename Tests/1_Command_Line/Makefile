all: check

.PHONY : clean

check: ../../Obj/archicheck clean
	@ echo archicheck return code test
	@ test ! `../../Obj/archicheck -I		>/dev/null 2>&1`
	@ test ! `../../Obj/archicheck -I qsdqjh	>/dev/null 2>&1`
	@ mkdir dir1
	@ test ! `../../Obj/archicheck -I dir1		>/dev/null 2>&1`
	@ test ! `../../Obj/archicheck -I dir2		>/dev/null 2>&1`
	@ test ! `../../Obj/archicheck -I dir1 --list_components qdfgh	>/dev/null 2>&1`
	@ echo OK

clean:
	@ ${RM} -rf dir1
