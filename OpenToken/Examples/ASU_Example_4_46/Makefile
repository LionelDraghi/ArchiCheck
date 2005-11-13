all : asu_example_4_46-run asu_example_4_46_rd-run

asu_example_4_46-run : asu_example_4_46.ads asu_example_4_46-run.adb
	gnatmake -I../../ asu_example_4_46-run

asu_example_4_46_rd-run : asu_example_4_46_rd.ads asu_example_4_46_rd-run.adb
	gnatmake -I../../ asu_example_4_46_rd-run

clean :
	$(RM) *.exe *.o *.ali