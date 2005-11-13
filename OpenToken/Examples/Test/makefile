###############################################################################
##
## Copyright (C) 1999 FlightSafety International and Ted Dennison
##
## This file is part of the OpenToken package.
##
## The OpenToken package is free software; you can redistribute it and/or
## modify it under the terms of the  GNU General Public License as published
## by the Free Software Foundation; either version 2, or (at your option)
## any later version. The OpenToken package is distributed in the hope that
## it will be useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for  more details.  You should have received
## a copy of the GNU General Public License  distributed with the OpenToken
## package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
## 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
##
## As a special exception,  if other files  instantiate  generics from this
## unit, or you link this unit with other files to produce an executable,
## this unit does not by itself cause the resulting executable to be
## covered by the GNU General Public License.  This exception does not
## however invalidate any other reasons why the executable file might be
## covered by the GNU Public License.
##
## Maintainer: Ted Dennison (dennison@telepath.com)
##
## Update History:
## $Log: makefile,v $
## Revision 1.5  2000/08/12 21:40:15  Ted
## Add an item to the temp_files list
##
## Revision 1.4  2000/08/07 00:16:16  Ted
## Change to use new child packages
##
## Revision 1.3  2000/02/05 04:04:54  Ted
## Add bracketed comment test.
##
## Revision 1.2  2000/01/27 21:17:25  Ted
## Add new tests.
## Build with full debug (for debugging when things don't work).
##
## Revision 1.1  1999/10/08 23:24:07  Ted
## Makefile for test programs
##
##
###############################################################################

###############################################################################
## Makefile for OpenToken's test suite
##
## In addition to the individual test targets, the following targets are 
## supported:
##    all   - (default) build and run the entire suite
##    clean - remove all files created by this makefile (or the running of a
##            test). 
##
## This file is designed to work with the gnat compiler. For other compilers
## you may have to "register" the source files somehow, and change the 
## ADAMAKE variable.
###############################################################################

###############################################################################
# Tunable section. The defaults are set for a Windows NT/Gnat environment.
###############################################################################
ADAMAKE = gnatmake -g -I../../   # Change to your compiler's build command
EXE_SUFFIX = .exe#              Change to your OS's executable extension

###############################################################################
# End of tunable section.
###############################################################################
Test_Exes = $(addsuffix $(EXE_SUFFIX), string_test-run token_analyzer_ctd-run token_list_test-run \
production_test-run bracketed_comment_test-run lookahead_test-run token_sequence_test-run \
token_selection_test-run token_list_test-run)
Temp_Files = Test.txt String_Test.txt Bracketed_Comment_Test.txt Lookahead_Test.txt

.phony : all clean

all : $(Test_Exes)
	string_test-run
	token_analyzer_ctd-run
	token_list_test-run
	production_test-run
	bracketed_comment_test-run
	lookahead_test-run
	token_sequence_test-run
	token_selection_test-run
	token_list_test-run

%$(EXE_SUFFIX) : %.adb
	$(ADAMAKE) $*

clean : 
	-$(RM) -f *.o *.ali b~* $(Test_Exes) $(Temp_Files)

