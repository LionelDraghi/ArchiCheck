with Ada.Text_IO;
with Ada.Command_Line;

with OpenToken.Text_Feeder.Text_IO;
with Ada_Lexer;                      use  Ada_Lexer;
with Archicheck.Source_List;

procedure Archicheck.Main is
   -- Global text file for reading parse data
   File : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));

   Ada.Text_IO.Set_Input (File);
   Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

   loop

      Tokenizer.Find_Next (Analyzer);

      Ada.Text_IO.Put_Line
        ("Found " & Ada_Token'Image (Tokenizer.ID (Analyzer)) &
         ' ' & Tokenizer.Lexeme (Analyzer));

      exit when Tokenizer.ID (Analyzer) = End_of_File_T;

   end loop;

end Archicheck.Main;
