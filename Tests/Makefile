all: check

.PHONY : clean

Tools/create_pkg:
	@ $(MAKE) --directory=Tools

check: archicheck Tools/create_pkg
	@ $(MAKE) check --directory=Return_Code
	@ $(MAKE) check --directory=Source_List
	@ $(MAKE) check --directory=Dependency_List
	@ $(MAKE) check --directory=Component_List
	@ $(MAKE) check --directory=Rules
	@ $(MAKE) check --directory=Child_Packages

clean:
	@ $(MAKE) clean --directory=Return_Code
	@ $(MAKE) clean --directory=Source_List
	@ $(MAKE) clean --directory=Dependency_List
	@ $(MAKE) clean --directory=Component_List
	@ $(MAKE) clean --directory=Rules
	@ $(MAKE) clean --directory=Child_Packages
	@ $(MAKE) clean --directory=Tools

