with Ada_Lexer;        use Ada_Lexer;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with OpenToken;
procedure Run_Ada_Lexer
is
   File : File_Type;
begin
   Open (File, In_File, Argument (1));

   Set_Input_Feeder (File);

   --  ada_lexer reports ? in comment as bad token!
   Bad_Token_on_Syntax_Error;

   loop
      exit when End_Of_File (File);
      Find_Next;
   end loop;
exception
when E : OpenToken.Syntax_Error =>
   Put_Line (Count'Image (Line (File)) & ": " & Ada.Exceptions.Exception_Message (E));

end Run_Ada_Lexer;
