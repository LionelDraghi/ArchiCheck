all: check

.PHONY : clean

check: archicheck
	@ $(MAKE) check --directory=Return_Code
	@ $(MAKE) check --directory=Source_List
	@ $(MAKE) check --directory=Dependency_List
	@ $(MAKE) check --directory=Component_List

clean:
	@ $(MAKE) clean --directory=Return_Code
	@ $(MAKE) clean --directory=Source_List
	@ $(MAKE) clean --directory=Dependency_List
	@ $(MAKE) clean --directory=Component_List

