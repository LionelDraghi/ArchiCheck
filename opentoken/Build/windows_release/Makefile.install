# install OpenToken in GNAT tree
# (ediff "../windows_release/Makefile.install" "../linux_release/Makefile.install")

# Where opentoken library should be installed
# Default is into current GNAT directory
INSTALL_DIR ?= $(dir $(shell which gnatls))..
prefix=$(INSTALL_DIR)

all: install

I_INC	= $(prefix)/include/opentoken
I_LIB	= $(prefix)/lib/opentoken
I_GPR	= $(prefix)/lib/gnat

# run this from the main Makefile to build the library
install:
	rm -rf $(I_INC)
	rm -rf $(I_LIB)
	mkdir -p $(I_INC)
	mkdir -p $(I_LIB)
	mkdir -p $(I_GPR)
	cp -p lib/*.ali $(I_LIB)
	chmod a-w $(I_LIB)/*.ali
	cp -p lib/libopentoken* $(I_LIB)
	chmod a-w $(I_LIB)/libopentoken*
	cp -p ../../*.ad[bs] $(I_INC)
	cp -p ../../Language_Lexers/*.ad[bs] $(I_INC)
	cp -p ../opentoken.gpr $(I_GPR)

# end of file
