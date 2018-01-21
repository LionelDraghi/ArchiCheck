--  Copyright (C) 2002 - 2005, 2008 - 2014 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Ada.Text_IO;
package body OpenToken.Production.Parser.LALR.Parser is

   --  Return the action for the given state index and terminal ID.
   --  The final action in the action list for a state (the error
   --  action) is returned if no other node matches ID.
   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token.Terminal_ID)
     return Parse_Action_Node_Ptr
   is
      use type Token.Terminal_ID;
      Action_Node : Action_Node_Ptr := Table (State).Action_List;
   begin
      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      return Action_Node.Action;
   end Action_For;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token.Token_ID)
     return State_Index
   is
      use type Token.Terminal_ID;
      Goto_Node : Goto_Node_Ptr := Table (State).Goto_List;
   begin
      while Goto_Node.Next /= null and Goto_Node.Symbol /= ID loop
         Goto_Node := Goto_Node.Next;
      end loop;

      return Goto_Node.State;
   end Goto_For;

   type Token_Array is array (Integer range <>) of Token.Token_ID;

   function Expecting (Table : in Parse_Table_Ptr; State : in State_Index) return Token_Array
   is
      Action : Action_Node_Ptr := Table (State).Action_List;
      Count  : Integer         := 0;
   begin
      loop
         exit when Action = null;

         Count  := Count + 1;
         Action := Action.Next;
      end loop;

      --  Last action is error; don't include it.
      declare
         Result : Token_Array (1 .. Count - 1);
      begin
         Action := Table (State).Action_List;
         for I in Result'Range loop
            Result (I) := Action.Symbol;
            Action     := Action.Next;
         end loop;
         return Result;
      end;
   end Expecting;

   function Names (Analyzer : in Tokenizer.Handle; Tokens : in Token_Array) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in Tokens'Range loop
         Result := Result & "'" & Analyzer.Name (Tokens (I));
         if I = Tokens'Last then
            Result := Result & "'";
         else
            Result := Result & "' or ";
         end if;

      end loop;
      return To_String (Result);
   end Names;

   procedure Reduce_Stack
     (Current_Parser : in Parser_Lists.Cursor;
      New_Token      : in Nonterminal.Handle;
      Action         : in Reduce_Action_Rec)
   is
      use type Nonterminal.Synthesize;

      Tokens : Token_List.Instance;
   begin
      --  Pop the indicated number of token states from the stack, and
      --  if not Pending call the production action routine to update
      --  New_Token.
      --
      --  If Pending, queue the action and tokens for later.

      if Action.Token_Count > 0 then
         for I in 1 .. Action.Token_Count loop
            --  Enqueue does not deep copy Token, but clean frees it
            Token_List.Enqueue (Tokens, Current_Parser.Pop.Token);
         end loop;
      end if;

      declare
         Action_Token : constant Parser_Lists.Action_Token := (Action, New_Token, Tokens);
      begin
         if Current_Parser.Active_Parser_Count > 1 then
            Current_Parser.Enqueue (Action_Token);
            if Trace_Parse > 0 then
               Ada.Text_IO.Put ("pending ");
               Parser_Lists.Put (Action_Token);
               Ada.Text_IO.Put_Line (" action count:" & Integer'Image (Current_Parser.Action_Token_Count));
            end if;
         else
            Action.Action (New_Token.all, Tokens, Token.ID (New_Token.all));
            if Trace_Parse > 0 then
               Parser_Lists.Put (Action_Token);
               Ada.Text_IO.New_Line;
            end if;
            Token_List.Clean (Tokens);
         end if;
      end;
   end Reduce_Stack;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Current_Token  : in Token.Handle;
      Table          : in Parse_Table)
   is begin
      if Trace_Parse > 0 then
         if Trace_Parse > 1 then
            Parser_Lists.Put_Top_10 (Current_Parser);
         end if;
         Ada.Text_IO.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              State_Image (Current_Parser.Peek.State) & ": " &
              Current_Token.Image & " : ");
         Put (Action);
         Ada.Text_IO.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Push ((Action.State, Token.Copy (Current_Token)));

      when Reduce =>
         declare
            New_Token : constant Nonterminal.Handle := new Nonterminal.Class'(Action.LHS.all);
         begin
            Reduce_Stack (Current_Parser, New_Token, Action);

            Current_Parser.Push
              ((State    => Goto_For
                  (Table => Table,
                   State => Current_Parser.Peek.State,
                   ID    => Token.ID (Action.LHS.all)),
                Token    => OpenToken.Production.Token.Handle (New_Token)));

            if Trace_Parse > 0 then
               Ada.Text_IO.Put_Line (" ... goto state " & State_Image (Current_Parser.Peek.State));
            end if;
         end;

      when Accept_It =>
         declare
            New_Token : constant Nonterminal.Handle := new Nonterminal.Class'(Action.LHS.all);
         begin
            Reduce_Stack
              (Current_Parser, New_Token, (Reduce, Action.LHS, Action.Action, Action.Index, Action.Token_Count));
         end;

      when Error =>
         null;

      end case;

      Current_Parser.Set_Verb (Action.Verb);
   end Do_Action;

   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : all Parsers.Verb return Accept, Shift, or Error - get a
   --  new token, execute Shift parsers, terminate Error parsers.
   --
   --  Reduce : some Parsers.Verb return Reduce - no new token,
   --  execute Reduce parsers.
   --
   --  Error : all Parsers.Verb return Error; report errors, terminate
   --  parse.
   function Parse_Verb (Parsers : in Parser_Lists.List) return Parse_Action_Verbs
   is
      Shift_Count  : Integer := 0;
      Accept_Count : Integer := 0;
      Error_Count  : Integer := 0;
   begin
      --  Cursor.Verb is the last action a parser took. If it was Shift,
      --  that parser used the input token, and should not be executed
      --  again until another input token is available, after all
      --  parsers have shifted the current token or terminated.
      for Cursor in Parsers.Iterate loop

         case Parser_Lists.Verb (Cursor) is
         when Shift =>
            Shift_Count := Shift_Count + 1;

         when Reduce =>
            return Reduce;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            Error_Count := Error_Count + 1;
         end case;
      end loop;

      if Parsers.Count = Accept_Count then
         return Accept_It;
      elsif Parsers.Count = Error_Count then
         return Error;
      elsif Parsers.Count = Shift_Count + Accept_Count + Error_Count then
         return Shift;
      else
         raise Programmer_Error;
      end if;
   end Parse_Verb;

   function Duplicate_State
     (Parsers        : aliased in out Parser_Lists.List;
      Current_Parser :         in     Parser_Lists.Cursor)
     return Boolean
   is
      use Parser_Lists;
   begin
      for I in Parsers.Iterate loop
         declare
            Cursor : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
         begin
            if Cursor /= Current_Parser and then Stack_Equal (Cursor, Current_Parser) then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Duplicate_State;

   procedure Execute_Pending (Current_Parser : in Parser_Lists.Cursor)
   is
      Action_Token : Parser_Lists.Action_Token;
   begin
      if Trace_Parse > 0 then
         Ada.Text_IO.Put_Line ("execute pending");
      end if;
      loop
         exit when Current_Parser.Action_Tokens_Empty;
         Action_Token := Current_Parser.Dequeue;
         Action_Token.Action.Action
           (Action_Token.New_Token.all, Action_Token.Tokens, Token.ID (Action_Token.New_Token.all));
         if Trace_Parse > 0 then
            --  Do Put after calling Action, so New_Token has result of Action
            Parser_Lists.Put (Action_Token);
            Ada.Text_IO.New_Line;
         end if;
         --  Action_Token.New_Token still on stack; freed later
         Token_List.Clean (Action_Token.Tokens);
      end loop;
   end Execute_Pending;

   overriding procedure Parse (Parser : in out Instance)
   is
      Parsers        : Parser_Lists.List := Parser_Lists.Initialize;
      Current_Verb   : Parse_Action_Verbs;
      Current_Token  : Token.Handle;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;
   begin

      loop
         --  exit on Accept_It action or syntax error.

         Current_Verb := Parse_Verb (Parsers);

         case Current_Verb is
         when Shift =>
            Parser.Analyzer.Find_Next;
            Token.Free (Current_Token);
            Current_Token := new Token.Class'(Token.Class (Parser.Analyzer.Get));

         when Accept_It =>
            --  Done.
            if Parsers.Count > 1 then
               raise Parse_Error with
                 Int_Image (Parser.Analyzer.Line) & ":" & Int_Image (Parser.Analyzer.Column) &
                 ": Ambiguous parse:" & Integer'Image (Parsers.Count) & " parsers active.";
            end if;
            --  FIXME: free everything
            return;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; report errors
            declare
               ID     : constant String := Token.Name (Current_Token.all);
               Lexeme : constant String := Parser.Analyzer.Lexeme;

               --  FIXME: merge expecting from all active parsers
               Expecting_Tokens : constant Token_Array := Expecting (Parser.Table, Parsers.First.Peek.State);
            begin
               --  FIXME: free everything
               raise Syntax_Error with
                 Int_Image (Parser.Analyzer.Line) & ":" & Int_Image (Parser.Analyzer.Column) &
                 ": Syntax error; expecting " & Names (Parser.Analyzer, Expecting_Tokens) &
                 "; found " & ID & " '" & Lexeme & "'";
            end;

         end case;

         Current_Parser := Parser_Lists.First (Parsers);
         loop
            exit when Current_Parser.Is_Done;

            --  All parsers reduce as much as possible, then shift
            --  Current_Token, then wait until all parsers have
            --  shifted it.

            if Current_Verb = Shift and Current_Parser.Verb = Error then
               if Trace_Parse > 0 then
                  Ada.Text_IO.Put_Line
                    (Integer'Image (Current_Parser.Label) & ": terminate (" &
                       Int_Image (Parsers.Count - 1) & " active)");
               end if;
               Current_Parser.Free;

               if Parsers.Count = 1 then
                  Execute_Pending (Parsers.First);
               end if;

            elsif Parser.Terminate_Same_State and then
              (Current_Verb = Shift and Duplicate_State (Parsers, Current_Parser))
            then
               if Trace_Parse > 0 then
                  Ada.Text_IO.Put_Line
                    (Integer'Image (Current_Parser.Label) & ": duplicate state; terminate (" &
                       Int_Image (Parsers.Count - 1) & " active)");
               end if;
               Current_Parser.Free;

               if Parsers.Count = 1 then
                  Execute_Pending (Parsers.First);
               end if;

            elsif Current_Parser.Verb = Current_Verb then

               Action := Action_For
                 (Table => Parser.Table.all,
                  State => Current_Parser.Peek.State,
                  ID    => Token.ID (Current_Token.all));

               if Action.Next /= null then
                  --  conflict; spawn a new parser
                  if Parsers.Count = Parser.Max_Parallel then
                     raise Parse_Error with
                       Int_Image (Parser.Analyzer.Line) & ":" & Int_Image (Parser.Analyzer.Column) &
                       ": too many parallel parsers required in grammar state" &
                       State_Index'Image (Current_Parser.Peek.State) &
                       "; simplify grammar, or increase max-parallel (" &
                       Integer'Image (Parser.Max_Parallel) & ")";

                  else
                     if Trace_Parse > 0 then
                        Ada.Text_IO.Put ("spawn parser from " & Int_Image (Current_Parser.Label));
                     end if;
                     Parsers.Prepend_Copy (Current_Parser);
                     if Trace_Parse > 0 then
                        Ada.Text_IO.Put_Line (" (" & Int_Image (Parsers.Count) & " active)");
                        Parser_Lists.Check_Action_Stack ("", Current_Parser);
                     end if;
                     Do_Action (Action.Next.Item, Parsers.First, Current_Token, Parser.Table.all);
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Current_Token, Parser.Table.all);

               Current_Parser.Next;
            else
               Current_Parser.Next;
            end if;
         end loop;
      end loop;
   end Parse;

   function Initialize
     (Analyzer             : in Tokenizer.Handle;
      Table                : in Parse_Table_Ptr;
      Max_Parallel         : in Integer := 15;
      Terminate_Same_State : in Boolean := False)
     return Instance
   is begin
      return (Analyzer, Table, Max_Parallel, Terminate_Same_State);
   end Initialize;


end OpenToken.Production.Parser.LALR.Parser;
