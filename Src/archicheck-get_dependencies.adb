-- -----------------------------------------------------------------------------
-- ArchiCheck, the software architecture compliance verifier
-- Copyright (C) 2005, 2006, 2009 - Lionel Draghi
-- This program is free software;
-- you can redistribute it and/or modify it under the terms of the GNU General
-- Public License Versions 3, refer to the COPYING file.
-- -----------------------------------------------------------------------------

-- Function: Archicheck.Get_Dependencies body

with Ada.Exceptions;
with Archicheck.IO;

with Ada.Text_IO;

with Ada_Lexer; use  Ada_Lexer;

function Archicheck.Get_Dependencies
  (Source_Name  : String) return Archicheck.Dependency_Lists.List
is

   -- Change default Debug parameter value to enable/disable Debug messages in this package
   -- -------------------------------------------------------------------------
   procedure Put_Debug_Line (Msg    : in String  := "";
                             Debug  : in Boolean := False;
                             Prefix : in String  := "Get_Dependencies") renames Archicheck.IO.Put_Debug_Line;
   procedure Put_Debug (Msg    : in String  := "";
                        Debug  : in Boolean := False;
                        Prefix : in String  := "Get_Dependencies") renames Archicheck.IO.Put_Debug;

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

      Put_Debug_Line (To_String (Name));

      return To_String (Name);
   end Get_Unit_Name;

   -- Iterate trough a list to set the field Unit_Name, and only this one.
   procedure Set_Unit_Name (List          : in out Dependency_Lists.List;
                            Unit_Name     : in String;
                            Specification : in Boolean) is
   begin
      for Dep of List loop
         Dep.Unit_Name     := To_Unbounded_String (Unit_Name);
         Dep.Specification := Specification;
      end loop;
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
            Put_Debug ("with ");
            Append
              (Tmp, (Unit_Name       => Null_Unbounded_String,
                     Depends_On_Unit => To_Unbounded_String (Get_Unit_Name),
                     Specification   => False));
         when Package_T =>
            Find_Next;
            if Token_ID = Body_T then
               Find_Next;
               Put_Debug ("package body ");
               Set_Unit_Name (Tmp, Get_Unit_Name, Specification => False);

            else
               Put_Debug ("package ");
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
            IO.Put_Error
              ("failed at line" & Integer'Image (Line) &
                 ", column" & Integer'Image (Column) &
                 " due to parse exception:");
            IO.Put_Error (Ada.Exceptions.Exception_Information (Error));
      end;

   end loop Source_Analysis;

   Ada.Text_IO.Close (File => File);

   return Dependencies;

end Archicheck.Get_Dependencies;
