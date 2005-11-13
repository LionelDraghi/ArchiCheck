###############################################################################
##
## Copyright (C) 1999,2000 Theodore Edward Dennison
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
## Revision 1.6  2000/08/07 00:12:58  Ted
## Use RM variable instead of hardcoded unix command
##
## Revision 1.5  2000/02/05 04:03:37  Ted
## Add Modula-3 lexer tester.
##
## Revision 1.4  2000/01/27 21:13:31  Ted
## Change default flags to optimize (for more speed out of the ada counter)
##
## Revision 1.3  1999/12/27 21:28:30  Ted
## Add html tester target
##
## Revision 1.2  1999/10/22 04:28:24  Ted
## Added Ada_Count. Modified to use pattern rule instead of one rule per target.
##
## Revision 1.1  1999/09/08 02:43:17  Ted
## Makefile for lexer examples
##
##############################################################################

GNAT_FLAGS = -O2

Main_Files = test_ada_lexer test_java_lexer ada_count test_html_lexer test_m3_lexer

all : $(Main_Files)

$(Main_Files) : % : %.adb
	gnatmake $< $(GNAT_FLAGS) -I../../ -I../../Language_Lexers/

clean : 
	-$(RM) *.exe *.o *.ali b~*
