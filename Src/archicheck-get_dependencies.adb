with Ada.Text_IO;

with OpenToken.Text_Feeder.Text_IO;
with Ada_Lexer;                      use  Ada_Lexer;
with Archicheck.Dependency_List;

function Archicheck.Get_Dependencies
  (Source_Name  : String) return Archicheck.Dependency_List.List
is
   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;
   Dependencies : Dependency_List.List;
   Tmp : Dependency_List.List;
   use Dependency_List;
   use Ada.Strings.Unbounded;

   procedure Set_Unit_Name (List :  Dependency_List.List;
                            Unit_Name : in String)
   is
      procedure Set (Dep : in out Dependency) is
      begin
         Dep.Unit_Name := To_Unbounded_String (Unit_Name);
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
      case Tokenizer.ID (Analyzer) is
         when With_T =>
            Tokenizer.Find_Next (Analyzer);
            -- Ada.Text_IO.Put_Line ("with " & Tokenizer.Lexeme (Analyzer));
            Append (Tmp,
                    (Unit_Name       => Null_Unbounded_String,
                     Depends_On_Unit => To_Unbounded_String
                       (Tokenizer.Lexeme (Analyzer))));
         when Package_T =>
            Tokenizer.Find_Next (Analyzer);
            if Tokenizer.ID (Analyzer) = Body_T then
               Tokenizer.Find_Next (Analyzer);
               declare
                  Name : constant String := Tokenizer.Lexeme (Analyzer);
               begin
                  -- Ada.Text_IO.Put_Line
                  --  ("package body " & Tokenizer.Lexeme (Analyzer));
                  Set_Unit_Name (Tmp, Name & " body         ");
--                    Append (Tmp,
--                            (Unit_Name       => To_Unbounded_String
--                               (Name & " body         "),
--                             Depends_On_Unit => To_Unbounded_String
--                               (Name & " specification")));
               end;

            else
               -- Ada.Text_IO.Put_Line
               --  ("package " & Tokenizer.Lexeme (Analyzer));
               Set_Unit_Name (Tmp,
                              Tokenizer.Lexeme (Analyzer) & " specification");
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
