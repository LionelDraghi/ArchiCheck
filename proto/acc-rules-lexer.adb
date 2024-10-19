-- Header TBD

with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body Acc.Rules.Lexer is

   Cursor         : Natural := 1;
   Line_Finished  : Boolean := False;
   The_Delimiters : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" ,;" & Ada.Characters.Latin_1.HT);

   package String_Arrays is new Ada.Containers.Indefinite_Vectors (Positive,
                                                                   String);
   Keywords       : constant String_Arrays.Vector := [1  => "a",
                                                      2  => "and",
                                                      3  => "are",
                                                      4  => "contains",
                                                      5  => "independent",
                                                      6  => "is",
                                                      7  => "files",
                                                      8  => "layer",
                                                      9  => "only",
                                                      10 => "may",
                                                      11 => "over",
                                                      12 => "use",
                                                      13 => "forbidden",
                                                      14 => "allowed"];

   -- --------------------------------------------------------------------------
   procedure Initialize_Cursor is begin
      Cursor := 1;
      Line_Finished := False;
   end Initialize_Cursor;

   -- --------------------------------------------------------------------------
   function Next_Token (Line     : access constant String;
                        Tok_Type : out Token_Type
                        -- Cursor : in out Cursor
                       ) return String is
      First : Positive;      -- Index of first character in token --***
      Last  : Natural := 0;  -- Index of last character in token (also used
      -- incremented by 1 as the starting point for next
      -- search).

      procedure Finish_Line is
      begin
         Line_Finished := True;
         Cursor := 1;
      end Finish_Line;

   begin
      Find_Token (Source => Line.all (Cursor .. Line'Last),
                  Set    => The_Delimiters,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);
      Cursor := (Natural'Min (Line.all'Length, Last + 1));

      if Is_A_Keyword (Line, First, Last) then
         Tok_Type := Keyword;
      else
         Tok_Type := Identifier;
      end if;

      if Last = Line.all'Last then
         -- It's the end of line
         -- -> next line.
         Finish_Line;
         -- Put_Line ("EOL");
      elsif (Line'Last >= First + 1 and then
             (Line.all (First .. First + 1) = "--"
             or else Line.all (First .. First + 1) = "//"))
        or Line.all (First .. First)     = "#"
      then
         -- Put_Line ("Comments remaining");
         -- there is only end of line comments left
         -- -> next line.
         Finish_Line;
         Tok_Type := Comment;
         return (Line.all (First .. Line.all'Last)); ------------------------
      end if;

      if Last = 0 then
         -- nothing found
         Finish_Line;
         Tok_Type := Empty;

         return ""; ---------------------------------------------------------
      else
         -- something found
         if Line.all (Last) = '.' then
            -- special rule : final '.' is ignored Fixme: to be documented
            return (Line.all (First .. Last - 1)); ---------------------------------
         else
            return (Line.all (First .. Last)); ---------------------------------
         end if;
      end if;

   end Next_Token;

   -- --------------------------------------------------------------------------
   function More_Token return Boolean is
   begin
      return not Line_Finished;
   end More_Token;

   -- --------------------------------------------------------------------------
   function Is_A_Keyword (S     : access constant String;
                          First  : Positive;
                          Last   : Natural := 0
                         ) return Boolean is
   begin
      return Keywords.Contains
        (Translate (Source  => S.all (First .. Last),
                    Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map));
   end Is_A_Keyword;


end Acc.Rules.Lexer;
