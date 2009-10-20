# common parts of makefiles for OpenToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : force

VPATH := ../..
VPATH += ../../Test
VPATH += ../../Examples/ASU_Example_3_6
VPATH += ../../Examples/ASU_Example_4_46
VPATH += ../../Examples/ASU_Example_5_10
VPATH += ../../Examples/Language_Lexer_Examples
VPATH += ../../Language_Lexers

tests : association_token_test-run.diff
tests : bracketed_comment_test-run.run
tests : enumerated_token_list_test.run
tests : lookahead_test-run.run
tests : name_token_test-run.diff
tests : production_test-run.run
tests : recognizer_based_integer_test.run
tests : recognizer_integer_test.run
tests : string_test-run.run
tests : string_token_test-run.diff
tests : test_all_harness.diff
tests : test_html_lexer_safe.diff
tests : test_html_lexer_safe-syntax_error.diff
tests : token_analyzer_ctd-run.run
tests : token_list_test-run.run
tests : token_selection_test-run.run
tests : token_sequence_test-run.run

examples : asu_example_3_6-run.run
examples : asu_example_4_46-run.run
examples : asu_example_4_46_rd-run.run
examples : asu_example_5_10-run.run
examples : asu_example_5_10_rd-run.run
examples : ada_count.run
examples : test_ada_lexer.run
examples : test_html_lexer_unsafe.run
examples : test_java_lexer.run
examples : test_m3_lexer.run

asu_example_3_6-run.run : asu_example_3_6-run.exe
	cd ../../Examples/ASU_Example_3_6; $(CURDIR)/asu_example_3_6-run.exe

asu_example_4_46-run.run : asu_example_4_46-run.exe
	cd ../../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46-run.exe

asu_example_4_46_rd-run.run : asu_example_4_46_rd-run.exe
	cd ../../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46_rd-run.exe

asu_example_5_10-run.run : asu_example_5_10-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10-run.exe < Example.txt

asu_example_5_10_rd-run.run : asu_example_5_10_rd-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_rd-run.exe < Example.txt

ada_count.run : ada_count.exe
	./ada_count.exe ../../Examples/Language_Lexer_Examples/ada_count.adb ../../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_ada_lexer.run : test_ada_lexer.exe
	./test_ada_lexer.exe ../../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_html_lexer_safe.out : test_html_lexer_safe.exe test_html_scan.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_safe-syntax_error.out : test_html_lexer_safe.exe test_html_scan-syntax_error.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_unsafe.run : test_html_lexer_unsafe.exe
	./test_html_lexer_unsafe.exe ../../Docs/opentoken.html

test_java_lexer.run : test_java_lexer.exe
	./test_java_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

# yes, we use the java source as a test for the m3 lexer. Close enough!
test_m3_lexer.run : test_m3_lexer.exe
	./test_m3_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

install: library
	make -f Makefile.install install

library:
	gnatmake -p -Popentoken_lib

clean :: test-clean
	rm -f *.diff *.exe *.out *.txt
	rm -f obj/*
	rm -rf lib/*

distclean :: clean
	rm -rf obj obj_tree

test-clean :
	rm -f *.diff *.out *.txt

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

%.exe : %.adb force; gnatmake -p -k -C -Popentoken_test.gpr $(GNATMAKE_ARGS) $* $(GNATMAKE_POST_ARGS)

%.out : %.exe ;	./$*.exe > $*.out 2>&1

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# end of file
