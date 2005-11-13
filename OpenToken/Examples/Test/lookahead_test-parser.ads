with OpenToken.Token.Enumerated;
with OpenToken.Token.Enumerated.Analyzer;

package Lookahead_Test.Parser is
   package Master_Example_Token is new Opentoken.Token.Enumerated (Example_Token_ID);
   package Tokenizer is new Master_Example_Token.Analyzer;

end Lookahead_Test.Parser;

