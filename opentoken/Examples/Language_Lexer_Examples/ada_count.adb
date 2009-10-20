-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Jim Hopper & Ted Dennison
--
-- This file uses the OpenToken package and is based upon one of its examples.
--
-- The Ada_Count program is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The Ada_Count program  is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada_Lexer; use Ada_Lexer;

procedure Ada_Count is

   --  The revision keyword automaticly gets the current CVS revision
   --  of this package when we check it out of clearcase. The revision
   --  string itself is the slice from the ':' to the '$'.
   Revision_Keyword : constant String := "$Revision: 1.4 $";
   Revision         : constant String := Revision_Keyword (12 .. Revision_Keyword'Length - 2);

   SLOC          : Integer := 0;
   Comment_Count : Integer := 0;
   Line_Count    : Integer := 0;

   ----------------------------------------------------------------------------
   --  Print a description of the proper usage of this program
   ----------------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line
        ("usage: " & Ada.Command_Line.Command_Name & " [-f]filename [[-f]filename ...]");
      Ada.Text_IO.Put_Line
        ("   Count the Software Lines Of Code in the listed source files.");
      Ada.Text_IO.Put_Line ("   Options: -ffilename");
      Ada.Text_IO.Put_Line ("               filename contains a return-separated list of source files.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("usage: " & Ada.Command_Line.Command_Name & " [--help]");
      Ada.Text_IO.Put_Line
        ("   Print out this message");
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("   For each source file counted, this program reports the number of lines of");
      Ada.Text_IO.Put_Line ("code, lines of text, and comments. A grand total for each of the above is");
      Ada.Text_IO.Put_Line ("reported at the end. A SLOC is considered an occurance of a semicolon (';')");
      Ada.Text_IO.Put_Line ("that is not part of another token (eg: not in a comment or string or character");
      Ada.Text_IO.Put_Line ("literal), and is not part of a parameter list. It is *not* able to automaticly");
      Ada.Text_IO.Put_Line ("count entire directories, as there is currently no portable way to get a");
      Ada.Text_IO.Put_Line ("directory listing. To use it in this way on Unix you can pipe it the output of");
      Ada.Text_IO.Put_Line ("a ""find"" command.");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("This program is copyright (C) 1999 Jim Hopper & Ted Dennison. It is distributed");
      Ada.Text_IO.Put_Line ("under the terms of the GMGPL as part of the OpenToken package. See the source");
      Ada.Text_IO.Put_Line ("header for full distribution and (un)warranty terms.");

   end Print_Usage;

   ----------------------------------------------------------------------------
   --  Parse the source file.
   --  This produces a count of all semicolon tokens that are not between a
   --  set of parentheses (and are not part of another Ada token like a
   --  string, of course). It also keeps track of the number of comment
   --  lines encountered.
   ----------------------------------------------------------------------------
   procedure Count (Filename : String) is

      Local_SLOC          : Integer := 0;
      Local_Comment_Count : Integer := 0;
      Paren_Count         : Integer := 0;

      --  Text file for reading parse data
      File : Ada.Text_IO.File_Type;
   begin

      --  Open the file for reading
      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.In_File,
         Name => Filename);

      Set_Input_Feeder (File);
      Bad_Token_on_Syntax_Error;

      --  Count statements and comments
      loop

         begin
            Find_Next;
            case Token_ID is
               when Semicolon_T =>
                  if Paren_Count = 0 then
                     Local_SLOC := Local_SLOC + 1;
                  end if;
               when Left_Parenthesis_T =>
                  Paren_Count := Paren_Count + 1;
               when Right_Parenthesis_T =>
                  Paren_Count := Paren_Count - 1;
               when Comment_T =>
                  Local_Comment_Count := Local_Comment_Count + 1;
               when others =>
                  null;
            end case;

         end;

         exit when Token_ID = End_of_File_T;

      end loop;

      --  Print the local results
      Ada.Text_IO.Put (Filename);
      Ada.Text_IO.Set_Col (43);
      Ada.Integer_Text_IO.Put (Item => Local_SLOC, Width => 10);
      Ada.Text_IO.Set_Col (56);
      Ada.Integer_Text_IO.Put (Item => Line - Line_Count, Width => 10);
      Line_Count := Line;
      Ada.Text_IO.Set_Col (67);
      Ada.Integer_Text_IO.Put (Item => Local_Comment_Count, Width => 10);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Close (File => File);

      --  Update the global results
      SLOC          := SLOC + Local_SLOC;
      Comment_Count := Comment_Count + Local_Comment_Count;

   end Count;

   procedure Count_From_File (File_Of_Filenames : in String) is
      File_List : Ada.Text_IO.File_Type;
      File_Name : String (1 .. 1024);
      Name_Size : Natural;
   begin

      --  Open the file for reading
      Ada.Text_IO.Open
        (File => File_List,
         Mode => Ada.Text_IO.In_File,
         Name => File_Of_Filenames);

      while not Ada.Text_IO.End_Of_File (File_List) loop
         Ada.Text_IO.Get_Line
           (File => File_List,
            Item => File_Name,
            Last => Name_Size
            );

         Count (File_Name (1 .. Name_Size));
      end loop;

      Ada.Text_IO.Close (File_List);
   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Close (File_List);
   end Count_From_File;

begin

   --  Verify the arguments
   if
     Ada.Command_Line.Argument_Count = 0 or else
     Ada.Command_Line.Argument (1) = "--help"
   then
      Print_Usage;
      return;
   end if;

   --  Modify the Ada syntax to report comments
   Set_Comments_Reportable (True);

   --  Print out a header line
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Open_Token Ada SLOC Counter " & Revision);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Filename");
   Ada.Text_IO.Set_Col (49);
   Ada.Text_IO.Put ("SLOC");
   Ada.Text_IO.Set_Col (55);
   Ada.Text_IO.Put ("Total Lines");
   Ada.Text_IO.Set_Col (69);
   Ada.Text_IO.Put ("Comments");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("----------------------------------------------------------------------------");

   --  Count from all the files listed on the command line
   for Arg_Num in 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Strings.Fixed.Head
        (Source => Ada.Command_Line.Argument (Arg_Num),
         Count  => 2) = "-f"
      then
         Count_From_File
           (Ada.Strings.Fixed.Tail
            (Source => Ada.Command_Line.Argument (Arg_Num),
             Count  => Ada.Command_Line.Argument (Arg_Num)'Length - 2)
            );

      else
         Count (Ada.Command_Line.Argument (Arg_Num));
      end if;
   end loop;

   --  Print out a trailer, and the accumulated totals
   Ada.Text_IO.Put_Line ("----------------------------------------------------------------------------");
   Ada.Text_IO.Set_Col (43);
   Ada.Integer_Text_IO.Put (Item => SLOC, Width => 10);
   Ada.Text_IO.Set_Col (56);
   Ada.Integer_Text_IO.Put (Item => Line, Width => 10);
   Ada.Text_IO.Set_Col (67);
   Ada.Integer_Text_IO.Put (Item => Comment_Count, Width => 10);
   Ada.Text_IO.New_Line;

end Ada_Count;
