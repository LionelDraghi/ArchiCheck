-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Jim Hopper & Ted Dennison
--
-- This file uses the OpenToken package and is based upon one of its examples.
--
-- The Ada_Count program is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The Ada_Count program  is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Jim Hopper (macconnect.com)
--
-- Update History:
-- $Log: ada_count.adb,v $
-- Revision 1.4  2000/01/27 21:11:35  Ted
-- Fix to use new token feeder objects
--
-- Revision 1.3  1999/12/27 23:05:01  Ted
-- Added file of filenames flag and usage printing
--
-- Revision 1.1  1999/10/22 04:26:30  Ted
-- A simple Ada SLOC and comment counter
--
--
-------------------------------------------------------------------------------

with Ada.Text_Io;
with Ada.Integer_Text_Io;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with OpenToken;
with OpenToken.Text_Feeder.Text_IO;

with Ada_Lexer;
use  Ada_Lexer;

procedure Ada_Count is

   -- The revision keyword automaticly gets the current CVS revision of this
   -- package when we check it out of clearcase. The revision string itself
   -- is the slice from the ':' to the '$'.
   Revision_Keyword : constant String := "$Revision: 1.4 $";
   Revision         : constant String := Revision_Keyword (12..Revision_Keyword'Length-2);

   SLOC          : Integer:= 0;
   Comment_Count : Integer:= 0;
   Line_Count    : Integer:= 0;

   ----------------------------------------------------------------------------
   -- Print a description of the proper usage of this program
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
   -- Parse the source file.
   -- This produces a count of all semicolon tokens that are not between a
   -- set of parentheses (and are not part of another Ada token like a
   -- string, of course). It also keeps track of the number of comment
   -- lines encountered.
   ----------------------------------------------------------------------------
   procedure Count(Filename : String) is

      Local_SLOC          : Integer:= 0;
      Local_Comment_Count : Integer:= 0;
      Paren_Count         : Integer:= 0;

      -- Text file for reading parse data
      File : Ada.Text_Io.File_Type;
   begin

      -- Open the file for reading
      Ada.Text_Io.Open
        (File => File,
         Mode => Ada.Text_Io.In_File,
         Name => Filename);

      Ada.Text_Io.Set_Input (File);
      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      -- Count statements and comments
      loop

         begin
            Tokenizer.Find_Next (Analyzer);
            case Tokenizer.ID (Analyzer) is
               when Semicolon_T =>
                  if Paren_Count = 0 then
                     Local_SLOC:= Local_SLOC + 1;
                  end if;
               when Left_Parenthesis_T =>
                  Paren_Count:= Paren_Count + 1;
               when Right_Parenthesis_T =>
                  Paren_Count:= Paren_Count - 1;
               when Comment_T =>
                  Local_Comment_Count:= Local_Comment_Count + 1;
               when others =>
                  null;
            end case;

         exception
            when OpenToken.Syntax_Error =>
               Ada.Text_Io.Put_Line
                 ("WARNING: Syntax error detected at" &
                  Integer'Image(Tokenizer.Line (Analyzer)) & ',' &
                  Integer'Image(Tokenizer.Column (Analyzer)));
         end;

         exit when Tokenizer.ID (Analyzer) = End_Of_File_T;

      end loop;

      -- Print the local results
      Ada.Text_Io.Put(Filename);
      Ada.Text_Io.Set_Col(43);
      Ada.Integer_Text_Io.Put (Item => Local_SLOC,Width => 10);
      Ada.Text_Io.Set_Col(56);
      Ada.Integer_Text_Io.Put (Item => Tokenizer.Line(Analyzer) - Line_Count, Width => 10);
      Line_Count := Tokenizer.Line(Analyzer);
      Ada.Text_Io.Set_Col(67);
      Ada.Integer_Text_Io.Put (Item => Local_Comment_Count,Width => 10);
      Ada.Text_Io.New_Line;
      Ada.Text_Io.Close(File => File);

      -- Update the global results
      SLOC          := SLOC + Local_SLOC;
      Comment_Count := Comment_Count + Local_Comment_Count;

   end Count;

   procedure Count_From_File (File_Of_Filenames : in String) is
      File_List : Ada.Text_Io.File_Type;
      File_Name : String (1..1024);
      Name_Size : Natural;
   begin

      -- Open the file for reading
      Ada.Text_Io.Open
        (File => File_List,
         Mode => Ada.Text_Io.In_File,
         Name => File_Of_Filenames);

      while not Ada.Text_Io.End_Of_File (File_List) loop
         Ada.Text_Io.Get_Line
           (File => File_List,
            Item => File_Name,
            Last => Name_Size
            );

         Count (File_Name (1..Name_Size));
      end loop;

      Ada.Text_Io.Close (File_List);
   exception
      when Ada.Text_Io.End_Error =>
         Ada.Text_Io.Close (File_List);
   end Count_From_File;

begin

   -- Verify the arguments
   if
     Ada.Command_Line.Argument_Count = 0 or else
     Ada.Command_Line.Argument(1) = "--help"
   then
      Print_Usage;
      return;
   end if;

   -- Modify the Ada syntax to report comments
   Ada_Lexer.Syntax (Comment_T).Recognizer.Report := True;

   -- Print out a header line
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Open_Token Ada SLOC Counter " & Revision);
   Ada.Text_IO.New_Line;
   Ada.Text_Io.Put("Filename");
   Ada.Text_Io.Set_Col(49);
   Ada.Text_Io.Put("SLOC");
   Ada.Text_Io.Set_Col(55);
   Ada.Text_Io.Put("Total Lines");
   Ada.Text_Io.Set_Col(69);
   Ada.Text_Io.Put("Comments");
   Ada.Text_Io.New_Line;
   Ada.Text_Io.Put_Line("----------------------------------------------------------------------------");

   -- Count from all the files listed on the command line
   for Arg_Num in 1..Ada.Command_Line.Argument_Count loop
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

   -- Print out a trailer, and the accumulated totals
   Ada.Text_Io.Put_Line("----------------------------------------------------------------------------");
   Ada.Text_Io.Set_Col(43);
   Ada.Integer_Text_Io.Put (Item => SLOC, Width => 10);
   Ada.Text_Io.Set_Col(56);
   Ada.Integer_Text_Io.Put (Item => Tokenizer.Line(Analyzer), Width => 10);
   Ada.Text_Io.Set_Col(67);
   Ada.Integer_Text_Io.Put (Item => Comment_Count, Width => 10);
   Ada.Text_Io.New_Line;

end Ada_Count;
