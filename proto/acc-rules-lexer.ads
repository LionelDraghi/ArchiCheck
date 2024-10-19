-- HEADER?
-- --------------------------------------------------------------------------
package Acc.Rules.Lexer is

   -- type Cursor is limited private;

   type Token_Type is (Keyword, Identifier, Comment, Empty);

   procedure Initialize_Cursor;

   function Next_Token (Line     : access constant String;
                        Tok_Type : out Token_Type)
                        return String;

   function More_Token return Boolean;

   function Is_A_Keyword (S     : access constant String;
                          First : Positive;
                          Last  : Natural := 0) return Boolean;

end Acc.Rules.Lexer;
