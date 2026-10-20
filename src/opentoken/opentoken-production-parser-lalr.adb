with Ada.Strings.Fixed;
with Ada.Text_IO;
package body OpenToken.Production.Parser.LALR is

   function State_Image (Item : in State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (State_Index'Image (Item), Both);
   end State_Image;

   procedure Put (Item : in Parse_Action_Rec)
   is
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Put
           ("reduce" & Integer'Image (Item.Token_Count) & " tokens to " & Token.Token_Image (Token.ID (Item.LHS.all)));
      when Accept_It =>
         Put ("accept it");
      when Error =>
         Put ("ERROR");
      end case;
   end Put;

end OpenToken.Production.Parser.LALR;
