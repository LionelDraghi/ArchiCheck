with OpenToken.Recognizer;
package Relop_Example_Token is

   type Instance is new OpenToken.Recognizer.Instance with private;

   ---------------------------------------------------------------------------
   --  This function will be called to create an Identifier token. Note that
   --  this is a simple recognizer, so Get doesn't need any parameters.
   ---------------------------------------------------------------------------
   function Get return Instance;

private

   type State_ID is (First_Char, Equal_or_Greater, Equal, Done);

   type Instance is new OpenToken.Recognizer.Instance with record
      State : State_ID := First_Char;
   end record;

   ---------------------------------------------------------------------------
   --  This procedure will be called when analysis on a new candidate string
   --  is started. The Token needs to clear its state (if any).
   ---------------------------------------------------------------------------
   overriding procedure Clear (The_Token : in out Instance);


   ---------------------------------------------------------------------------
   --  This procedure will be called to perform further analysis on a token
   --  based on the given next character.
   ---------------------------------------------------------------------------
   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out OpenToken.Recognizer.Analysis_Verdict);

end Relop_Example_Token;

