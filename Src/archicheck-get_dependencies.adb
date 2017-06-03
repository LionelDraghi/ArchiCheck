-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------


-- Function: Archicheck.Get_Dependencies body

with Ada.Exceptions;
with Ada.Text_IO;

with Ada_Lexer;                      use  Ada_Lexer;

function Archicheck.Get_Dependencies
  (Source_Name  : String) return Archicheck.Dependency_Lists.List
is
   Debug : constant Boolean := False;

   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;
   Dependencies : Dependency_Lists.List;
   Tmp : Dependency_Lists.List;
   use Dependency_Lists;
   use Ada.Strings.Unbounded;

   function Get_Unit_Name return String is
      Name : Unbounded_String := Null_Unbounded_String;
   begin
      Name := To_Unbounded_String (Lexeme);
      loop
         Find_Next;
         if Token_ID = Dot_T then
            Find_Next;
            Name := Name & "." & Lexeme;
         else
            exit;
         end if;

      end loop;

      if Debug then Ada.Text_IO.Put_Line (To_String (Name)); end if;

      return To_String (Name);
   end Get_Unit_Name;

   -- iterate trough a list to set the field Unit_Name,
   -- and only this one.
   procedure Set_Unit_Name (List          : in out Dependency_Lists.List;
                            Unit_Name     : in String;
                            Specification : in Boolean)
   is
      procedure Set (Dep : in out Dependency) is
      begin
         Dep.Unit_Name     := To_Unbounded_String (Unit_Name);
         Dep.Specification := Specification;
      end Set;

      procedure Set_Name (Position : Cursor) is
      begin
         Update_Element (List, Position, Set'Access);
      end Set_Name;

   begin
      Iterate (List, Set_Name'Access);
   end Set_Unit_Name;

begin
   Ada.Text_IO.Open (File => File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Source_Name);
   Set_Input_Feeder (File);

   Source_Analysis : loop
      begin
         -- The withed units are first stored in
         -- a Tmp dependency list, with the Unit_Name left blank,
         -- When the package name is meet, the Tmp list is modified to
         -- set the Unit_Name, then moved to the returned dependency list.
         -- Limitation: only the fist Ada unit per source
         -- Limitation: only packages are taken into account

         Find_Next;
         case Token_ID is
         when With_T =>
            Find_Next;
            if Debug then Ada.Text_IO.Put ("with "); end if;
            Append
              (Tmp, (Unit_Name       => Null_Unbounded_String,
                     Depends_On_Unit => To_Unbounded_String (Get_Unit_Name),
                     Specification   => False));
         when Package_T =>
            Find_Next;
            if Token_ID = Body_T then
               Find_Next;
               if Debug then Ada.Text_IO.Put ("package body "); end if;
               Set_Unit_Name (Tmp, Get_Unit_Name, Specification => False);

            else
               if Debug then Ada.Text_IO.Put ("package "); end if;
               Set_Unit_Name (Tmp,
                              Get_Unit_Name, Specification => True);
            end if;
            Move (Source => Tmp, Target => Dependencies);

            exit Source_Analysis;
            when others => null;
         end case;

         exit Source_Analysis when Token_ID = End_of_File_T;
      exception
         when Error : others =>
            Ada.Text_IO.Put_Line
              ("failed at line" & Integer'Image (Line) &
               ", column" & Integer'Image (Column) &
               " due to parse exception:");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      end;

   end loop Source_Analysis;

   Ada.Text_IO.Close (File => File);

   return Dependencies;

end Archicheck.Get_Dependencies;
