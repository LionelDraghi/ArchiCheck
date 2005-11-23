all: check

.PHONY : clean


check: archicheck
	@ echo component list test 1
	@ > expected_component_list.1
	@ echo "App contains Main"	>> expected_component_list.1

	@ > rules.1
	@ echo "App contains Main" 	>> rules.1

	@ archicheck --list_components rules.1 > component_list.1
	@ diff expected_component_list.1 component_list.1
	@ echo OK

	@ echo component list test 2
	@ > expected_component_list.2
	@ echo "App contains Main"			>> expected_component_list.2
	@ echo "GUI contains Gtk and Glib and Pango"	>> expected_component_list.2

	@ > rules.2
	@ echo "App contains Main"	>> rules.2
	@ echo "GUI contains Gtk"	>> rules.2
	@ echo "GUI contains Glib"	>> rules.2
	@ echo "GUI contains Pango"	>> rules.2

	@ archicheck --list_components rules.2 > component_list.2
	@ diff expected_component_list.2 component_list.2
	@ echo OK

	@ echo component list test 3
	@ > expected_component_list.3
	@ echo "App contains Main"			>> expected_component_list.3
	@ echo "DB contains DB.IO and DB.Query"		>> expected_component_list.3
	@ echo "GUI contains Gtk and Glib and Pango"	>> expected_component_list.3
	@ > rules.3
	@ echo "App contains Main"		>> rules.3
	@ echo "GUI contains Gtk, Glib"		>> rules.3
	@ echo "GUI contains also Pango;"	>> rules.3
	@ echo "DB contains DB.IO and DB.Query"	>> rules.3

	@ archicheck --list_components rules.3 > component_list.3
	@ diff expected_component_list.3 component_list.3
	@ echo OK

clean:
	@ ${RM} -rf expected_component_list.? component_list.? rules.?

