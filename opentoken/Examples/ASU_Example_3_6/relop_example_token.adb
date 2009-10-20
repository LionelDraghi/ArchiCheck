package body Relop_Example_Token is

   ---------------------------------------------------------------------------
   --  This procedure will be called when analysis on a new candidate string
   --  is started. The Token needs to clear its state (if any).
   ---------------------------------------------------------------------------
   overriding procedure Clear (The_Token : in out Instance) is
   begin
      The_Token.State := First_Char;
   end Clear;

   ---------------------------------------------------------------------------
   --  This procedure will be called to create a Relop token recognizer
   ---------------------------------------------------------------------------
   function Get return Instance is
   begin
      return (Report => True,
              State  => First_Char);
   end Get;

   --------------------------------------------------------------------------
   --  This procedure will be called to perform further analysis on a token
   --  based on the given next character.
   ---------------------------------------------------------------------------
   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out OpenToken.Recognizer.Analysis_Verdict)
   is begin

      case The_Token.State is

         when First_Char =>
            --  If the first char is a <, =, or >, its a match
            case Next_Char is
               when '<' =>
                  Verdict         := OpenToken.Recognizer.Matches;
                  The_Token.State := Equal_or_Greater;

               when '>' =>
                  Verdict         := OpenToken.Recognizer.Matches;
                  The_Token.State := Equal;


               when '=' =>
                  Verdict         := OpenToken.Recognizer.Matches;
                  The_Token.State := Done;

               when others =>
                  Verdict         := OpenToken.Recognizer.Failed;
                  The_Token.State := Done;
            end case;

         when Equal_or_Greater =>

            --  If the next char is a > or =, its a match
            case Next_Char is
               when '>' | '=' =>
                  Verdict         := OpenToken.Recognizer.Matches;
                  The_Token.State := Done;

               when others =>
                  Verdict         := OpenToken.Recognizer.Failed;
                  The_Token.State := Done;
            end case;

         when Equal =>

            --  If the next char is a =, its a match
            if Next_Char = '=' then
               Verdict         := OpenToken.Recognizer.Matches;
               The_Token.State := Done;
            else
               Verdict         := OpenToken.Recognizer.Failed;
               The_Token.State := Done;
            end if;

         when Done =>
            Verdict := OpenToken.Recognizer.Failed;
      end case;
   end Analyze;

end Relop_Example_Token;
