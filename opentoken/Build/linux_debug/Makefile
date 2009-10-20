all: dirs tests examples

# ../../Test/test_one_harness.adb
#one : test_one_harness.run
one : asu_example_5_10_rd-run.run

trace :
	addr2line -e token_selection_test-run.exe 0x805b2e7 0x805cfd3 0x8067ed9 0x804d36b 0x40345453

dirs :: lib

lib:
	@mkdir -p lib

clean ::
	@rm -rf lib/*

distclean ::
	@rm -rf lib

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 4.3.2

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
