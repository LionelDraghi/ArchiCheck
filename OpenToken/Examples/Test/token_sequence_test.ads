with OpenToken.Token.Sequence;
with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;

package Token_Sequence_Test is

   type Token_Ids is (Do_ID, Several, Things, Int, Times, In_Id, A_Id, Row, EOF, Whitespace);

   package Terminal_Token is new OpenToken.Token.Enumerated(Token_IDs);
   package Tokenizer is new Terminal_Token.Analyzer;

   Syntax : constant Tokenizer.Syntax :=
     (Do_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("do")),
      Several    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("several")),
      Things     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("things")),
      Int        => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
      Times      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("times")),
      In_Id      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("in")),
      A_Id       => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("a")),
      Row        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get("row")),
      EOF        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                   (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Do_Keyword      : Terminal_Token.Handle := Syntax(Do_Id).Token_Handle;
   Several_Keyword : Terminal_Token.Handle := Syntax(Several).Token_Handle;
   Things_Keyword  : Terminal_Token.Handle := Syntax(Things).Token_Handle;
   Int_Literal     : Terminal_Token.Handle := Syntax(Int).Token_Handle;
   Times_Keyword   : Terminal_Token.Handle := Syntax(Times).Token_Handle;
   In_Keyword      : Terminal_Token.Handle := Syntax(In_ID).Token_Handle;
   A_Keyword       : Terminal_Token.Handle := Syntax(A_ID).Token_Handle;
   Row_Keyword     : Terminal_Token.Handle := Syntax(Row).Token_Handle;

   Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

end Token_Sequence_Test;
