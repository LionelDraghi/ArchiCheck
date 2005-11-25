with Ada.Text_IO;

with OpenToken.Text_Feeder.Text_IO;
with Ada_Lexer;                      use  Ada_Lexer;
with Archicheck.Dependency_Lists;

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
      Name := To_Unbounded_String (Tokenizer.Lexeme (Analyzer));
      loop
--           Ada.Text_IO.Put_Line
--             ("look_Ahead = "
--              & Ada_Lexer.Ada_Token'Image (Tokenizer.ID (Analyzer))
--              & " -> " & Tokenizer.Lexeme (Analyzer));
         Tokenizer.Find_Next (Analyzer, Look_Ahead => True);
         if Tokenizer.ID (Analyzer) = Dot_T then
            Tokenizer.Find_Next (Analyzer);
            Name := Name & "." & Tokenizer.Lexeme (Analyzer);
         else
            exit;
         end if;

      end loop;

      if Debug then Ada.Text_IO.Put_Line (To_String (Name)); end if;

      return To_String (Name);
   end Get_Unit_Name;

   -- iterate trough a list to set the field Unit_Name,
   -- and only this one.
   procedure Set_Unit_Name (List          : in Dependency_Lists.List;
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
         Update_Element (Position, Set'Access);
      end Set_Name;

   begin
      Iterate (List, Set_Name'Access);
   end Set_Unit_Name;

begin
   Ada.Text_IO.Open (File => File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Source_Name);
   Ada.Text_IO.Set_Input (File);
   Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

   Source_Analysis : loop
      -- The withed units are first stored in
      -- a Tmp dependency list, with the Unit_Name left blank,
      -- When the package name is meet, the Tmp list is modified to
      -- set the Unit_Name, then moved to the returned dependency list.
      -- limitation : only the fist Ada unit per source
      -- limitation : only packages are taken into account

      Tokenizer.Find_Next (Analyzer);
--        Ada.Text_IO.Put_Line
--          (Ada_Lexer.Ada_Token'Image (Tokenizer.ID (Analyzer))
--           & " -> " & Tokenizer.Lexeme (Analyzer));
       case Tokenizer.ID (Analyzer) is
         when With_T =>
            Tokenizer.Find_Next (Analyzer);
            if Debug then Ada.Text_IO.Put ("with "); end if;
            Append
              (Tmp, (Unit_Name       => Null_Unbounded_String,
                     Depends_On_Unit => To_Unbounded_String (Get_Unit_Name),
                     Specification   => False));
         when Package_T =>
            Tokenizer.Find_Next (Analyzer);
            if Tokenizer.ID (Analyzer) = Body_T then
               Tokenizer.Find_Next (Analyzer);
               if Debug then Ada.Text_IO.Put ("package body "); end if;
               Set_Unit_Name (Tmp, Get_Unit_Name, Specification => False);
               -- Append (Tmp,
               --  (Unit_Name       => To_Unbounded_String
               --   (Name & " body         "),
               --   Depends_On_Unit => To_Unbounded_String
               --  (Name & " specification")));

            else
               if Debug then Ada.Text_IO.Put ("package "); end if;
               Set_Unit_Name (Tmp,
                              Get_Unit_Name, Specification => True);
            end if;
            Move (Source => Tmp, Target => Dependencies);

            exit Source_Analysis;
         when others => null;
       end case;

      exit Source_Analysis when Tokenizer.ID (Analyzer) = End_of_File_T;

   end loop Source_Analysis;

   Ada.Text_IO.Close (File => File);

   return Dependencies;

end Archicheck.Get_Dependencies;
