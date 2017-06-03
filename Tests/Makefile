all: check

.PHONY : clean

Tools/create_pkg:
	@ $(MAKE) --directory=Tools

check: archicheck Tools/create_pkg
	@ $(MAKE) check --directory=1_Command_Line
	@ $(MAKE) check --directory=2_Source_List
	@ $(MAKE) check --directory=3_Dependency_List
	@ $(MAKE) check --directory=4_Component_List
	@ $(MAKE) check --directory=5_Layers
	@ $(MAKE) check --directory=6_Child_Packages
	#	@ $(MAKE) check --directory=7_Globbing_Characters

clean:
	@ $(MAKE) clean --directory=Tools

	@ $(MAKE) clean --directory=1_Command_Line
	@ $(MAKE) clean --directory=2_Source_List
	@ $(MAKE) clean --directory=3_Dependency_List
	@ $(MAKE) clean --directory=4_Component_List
	@ $(MAKE) clean --directory=5_Layers
	@ $(MAKE) clean --directory=6_Child_Packages
	#	@ $(MAKE) clean --directory=7_Globbing_Characters

