with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;

package Token_List_Test is

   type Token_IDs is (Int, Comma, EOF, Whitespace);

   package Terminal_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Terminal_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (Int        => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
      Comma      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      EOF        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                   (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

end Token_List_Test;
