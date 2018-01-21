-------------------------------------------------------------------------------
--
--  Copyright (C) 2002, 2003, 2009, 2012 - 2014 Stephe Leake
--  Copyright (C) 1999, 2000 FlightSafety International and Ted Dennison
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
--
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body OpenToken.Token.Enumerated.Analyzer is

   type Match_List is array (Syntax_ID) of Recognizer.Analysis_Verdict;

   --  Routines to handle indexes in a circular ring buffer

   --  Increment Index by Amount, wrapping at end of buffer.
   function Increment_Buffer_Index
     (Max_Buffer_Size : in Integer;
      Index           : in Integer;
      Amount          : in Integer)
     return Natural
   is begin
      return ((Index + Amount - 1) mod Max_Buffer_Size) + 1;
   end Increment_Buffer_Index;

   -----------------------------------------------------------------------
   --  Return True if the given index designates a valid element of
   --  the instance's ring buffer.
   -----------------------------------------------------------------------
   function In_Buffer
     (Index    : Integer;
      Analyzer : Instance)
     return Boolean
   is begin

      --  It is not in the buffer if the Index is outside the string or the string is empty
      if Index not in Analyzer.Buffer'Range or Analyzer.Buffer_Size = 0 then
         return False;
      end if;

      if Analyzer.Buffer_Head <= Analyzer.Buffer_Tail then
         --  It is in the buffer if its between the head and the tail
         return Index in Analyzer.Buffer_Head .. Analyzer.Buffer_Tail;
      else
         --  It is in the buffer if it is not bounded by the tail and the head
         return Index not in Analyzer.Buffer_Tail + 1 .. Analyzer.Buffer_Head - 1;
      end if;

   end In_Buffer;

   -----------------------------------------------------------------------
   --  Get more text for the analyzer's buffer. The actual amount
   --  retrieved depends on the amount available from the text feeder
   --  and either the amount of space left in the buffer, or the
   --  distance from the tail to the buffer's wrap point.
   -----------------------------------------------------------------------
   procedure Get_More_Text (Analyzer : in out Instance)
   is
      First_New_Char : constant Natural := Increment_Buffer_Index
        (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Tail, 1);
   begin
      if Analyzer.Buffer_Head > First_New_Char then

         OpenToken.Text_Feeder.Get
           (Feeder   => Analyzer.Feeder.all,
            New_Text => Analyzer.Buffer (First_New_Char .. Analyzer.Buffer_Head - 1),
            Text_End => Analyzer.Buffer_Tail);
      else
         OpenToken.Text_Feeder.Get
           (Feeder   => Analyzer.Feeder.all,
            New_Text => Analyzer.Buffer (First_New_Char .. Analyzer.Buffer'Last),
            Text_End => Analyzer.Buffer_Tail);
         --  FIXME: more space in buffer'first .. head! test end of
         --  file in first and second read.
      end if;
      Analyzer.Buffer_Size := Analyzer.Buffer_Size + Analyzer.Buffer_Tail - First_New_Char + 1;

   end Get_More_Text;

   -----------------------------------------------------------------------
   --  Return the number of EOLs in the first Length characters in the
   --  Analyzer's buffer
   -----------------------------------------------------------------------
   function EOLs_Buffered
     (Analyzer : in Instance;
      Length   : in Natural)
     return Natural
   is
      Slice_Tail : constant Natural := Increment_Buffer_Index
        (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Head, Length - 1);
      EOL_String : constant String  := (1 => EOL_Character);
   begin
      if Slice_Tail < Analyzer.Buffer_Head then
         return Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Analyzer.Buffer'Last),
            Pattern => EOL_String)
           +
           Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer'First .. Slice_Tail),
            Pattern => EOL_String);
      else
         return
           Ada.Strings.Fixed.Count
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Slice_Tail),
            Pattern => EOL_String);
      end if;
   end EOLs_Buffered;

   -----------------------------------------------------------------------
   --  Returns the number of characters found after the last EOL in
   --  the next Length-1 characters after the buffer's head.
   --
   --  Needed for a multi-line comment:
   --  /* Foo
   --     Bar */ next-token
   -----------------------------------------------------------------------
   function Characters_After_Last_EOL
     (Analyzer : in Instance;
      Length   : in Natural)
     return Natural
   is
      --  Buffer_Head has not been incremented across recognized token
      Slice_Tail : constant Natural := Increment_Buffer_Index
        (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Head, Length - 1);
      EOL_String : constant String  := (1 => EOL_Character);

      Last_EOL : Natural;
   begin

      if Length = 1 then
         --  Length must include an EOL, so no characters after it
         return 0;

      elsif Slice_Tail < Analyzer.Buffer_Head then
         --  Length crosses end of buffer, wraps to beginning
         Last_EOL := Ada.Strings.Fixed.Index
           (Source  => Analyzer.Buffer (Analyzer.Buffer'First .. Slice_Tail),
            Pattern => EOL_String,
            Going   => Ada.Strings.Backward);

         if Last_EOL = 0 then

            Last_EOL := Ada.Strings.Fixed.Index
              (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Analyzer.Buffer'Last),
               Pattern => EOL_String,
               Going   => Ada.Strings.Backward);

         end if;
      else

         Last_EOL := Ada.Strings.Fixed.Index
           (Source  => Analyzer.Buffer (Analyzer.Buffer_Head .. Slice_Tail),
            Pattern => EOL_String,
            Going   => Ada.Strings.Backward);

      end if;

      return Increment_Buffer_Index (Analyzer.Max_Buffer_Size, Slice_Tail, -Last_EOL);

   end Characters_After_Last_EOL;

   -------------------------------------------------------------------------
   --  Find all the characters that *aren't* part of a token match.
   --
   --  This routine should be called when the first character in the
   --  analyzer's buffer is unmatchable. It succesively checks each
   --  character thereafter until it finds one that *does* start a
   --  valid token.
   --
   --  Last_Unmatched will be set to the index of the last unmatchable
   --  character.
   -----------------------------------------------------------------------
   procedure Find_Non_Match
     (Unmatched_Length : out    Natural;
      Analyzer         : in out Instance)
   is

      --  The table of token matches
      Match : Match_List;

      Possible_Matches : Boolean;
      Current_Char     : Integer;

      use type OpenToken.Recognizer.Analysis_Verdict;
   begin

      --  Loop to find unrecognized characters
      Unmatched_Length := 1;

      Check_For_Unrecognized : loop

         --  Loop to see if the next chacter past the last unmatched one starts a valid token
         Current_Char := Increment_Buffer_Index (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Head, Unmatched_Length);

         --  Clear the state of all the tokens
         for Token_Index in Analyzer.Syntax_List'Range loop
            Recognizer.Clear (Analyzer.Syntax_List (Token_Index).Recognizer.all);
         end loop;
         Match := (others => Recognizer.So_Far_So_Good);

         Check_For_Match : loop

            --  Get more data when we run out
            if not In_Buffer (Index => Current_Char, Analyzer => Analyzer) then
               Get_More_Text (Analyzer);
            end if;

            --  Loop to see if this character starts a match on any
            --  token. We will assume that there are no possible
            --  matches until proven otherwise.
            Possible_Matches := False;

            for Token_Index in Analyzer.Syntax_List'Range loop

               if Match (Token_Index) /= Recognizer.Failed then
                  Recognizer.Analyze
                    (The_Token => Analyzer.Syntax_List (Token_Index).Recognizer.all,
                     Next_Char => Analyzer.Buffer (Current_Char),
                     Verdict   => Match (Token_Index));
               end if;

               case Match (Token_Index) is
               when Recognizer.Matches =>
                  --  We found a match, quit.
                  return;

               when Recognizer.So_Far_So_Good =>
                  --  We *could* have a match, check the next character
                  Possible_Matches := not (Current_Char = Analyzer.Buffer_Tail and then
                                             OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all));

               when others =>
                  null;
               end case;

            end loop;

            exit Check_For_Match when not Possible_Matches;

            Current_Char := Increment_Buffer_Index (Analyzer.Max_Buffer_Size, Current_Char, 1);

         end loop Check_For_Match;

         Unmatched_Length := Unmatched_Length + 1;

         exit Check_For_Unrecognized when Current_Char = Analyzer.Buffer_Tail and then
           OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all);

      end loop Check_For_Unrecognized;

   end Find_Non_Match;

   --  Find the longest matching character sequence in the Analyzer's
   --  buffer that matches a token. If the buffer runs out of
   --  characters during this process, it will be refilled from the
   --  Analyzer's text feeder function.
   procedure Find_Best_Match
     (Analyzer          : in out Instance;
      Best_Match_Token  :    out Syntax_ID;
      Best_Match_Length :    out Natural)
   is
      Match                 : Match_List := (others => Recognizer.So_Far_So_Good);
      More_Possible_Matches : Boolean    := True;

      Current_Char : Integer := Analyzer.Buffer_Head;

      use type OpenToken.Recognizer.Analysis_Verdict;
   begin

      for Token_Index in Analyzer.Syntax_List'Range loop
         Recognizer.Clear (Analyzer.Syntax_List (Token_Index).Recognizer.all);
      end loop;

      Best_Match_Length := 0;

      while More_Possible_Matches loop

         if Best_Match_Length = Analyzer.Max_Buffer_Size then
            raise Programmer_Error with
              "token larger than buffer size of" & Integer'Image (Analyzer.Max_Buffer_Size);
         end if;

         if not In_Buffer (Current_Char, Analyzer) then
            Get_More_Text (Analyzer);
         end if;

         More_Possible_Matches := False;

         for Token_Index in Analyzer.Syntax_List'Range loop

            if Match (Token_Index) /= Recognizer.Failed then

               Recognizer.Analyze
                 (The_Token => Analyzer.Syntax_List (Token_Index).Recognizer.all,
                  Next_Char => Analyzer.Buffer (Current_Char),
                  Verdict   => Match (Token_Index));

               if Match (Token_Index) = Recognizer.Matches and
                 Best_Match_Length < Increment_Buffer_Index
                   (Analyzer.Max_Buffer_Size, Current_Char, 1 - Analyzer.Buffer_Head)
               then
                  Best_Match_Token  := Token_Index;
                  Best_Match_Length := Increment_Buffer_Index
                    (Analyzer.Max_Buffer_Size, Current_Char, 1 - Analyzer.Buffer_Head);
               end if;

               --  Exit (returning Best_Match) when all have failed or
               --  there is no more text to try for the ones that are
               --  not failing.
               if Match (Token_Index) /= Recognizer.Failed then
                  More_Possible_Matches := not
                    (Current_Char = Analyzer.Buffer_Tail and then
                       OpenToken.Text_Feeder.End_Of_Text (Analyzer.Feeder.all));
               end if;
            end if;
         end loop;

         Current_Char := Increment_Buffer_Index (Analyzer.Max_Buffer_Size, Current_Char, 1);

      end loop;
   end Find_Best_Match;

   function Null_Analyzer return Instance
   is
      New_Analyzer : Instance (1);
   begin
      New_Analyzer.Lookahead_Count := 0; -- avoid complaint about "read but never assigned"
      return New_Analyzer;
   end Null_Analyzer;

   function Initialize
     (Language_Syntax : in Syntax;
      Feeder          : in Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                     := 1024;
      First_Column    : in Integer                     := 1)
     return Handle
   is
      New_Analyzer : constant Handle := Initialize
        (Language_Syntax, Terminal_ID'First, Feeder, Buffer_Size, First_Column);
   begin
      New_Analyzer.Has_Default := False;

      return New_Analyzer;
   end Initialize;

   function Initialize
     (Language_Syntax : in Syntax;
      Default         : in Terminal_ID;
      Feeder          : in Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                     := 1024;
      First_Column    : in Integer                     := 1)
     return Handle
   is
      New_Analyzer : constant Handle := new Instance (Buffer_Size);
   begin
      --  Initialize the syntax
      New_Analyzer.Syntax_List    := Language_Syntax;
      for ID in Syntax'Range loop
         if New_Analyzer.Syntax_List (ID).Recognizer = null then
            raise Grammar_Error with "no recognizer for " & Token_Image (ID);
         end if;
         --  If Language_Syntax was created with the Get in this package,
         --  using the default New_Token parameter, the Token IDs are all
         --  Token_ID'First, which is wrong. So fix that now.
         New_Analyzer.Syntax_List (ID).Token_Handle.ID := ID;
      end loop;

      New_Analyzer.Feeder        := Feeder;
      New_Analyzer.Has_Default   := True;
      New_Analyzer.Default_Token := Default;
      New_Analyzer.First_Column  := First_Column;

      New_Analyzer.Line          := 1;
      New_Analyzer.Column        := New_Analyzer.First_Column;
      New_Analyzer.Lexeme_Head   := 1;
      New_Analyzer.Lexeme_Tail   := 0;
      New_Analyzer.Last_Token_ID := Default;

      --  This is not literally true, but it does mean the lexeme and
      --  recognizer are invalid.
      New_Analyzer.Read_From_Lookahead := True;

      New_Analyzer.Buffer_Head            := New_Analyzer.Buffer'First;
      New_Analyzer.Buffer_Tail            := New_Analyzer.Buffer'Last;
      New_Analyzer.Buffer_Size            := 0;
      New_Analyzer.Buffer_Head_Source_Pos := 1;
      New_Analyzer.Next_Line              := 1;
      New_Analyzer.Next_Column            := New_Analyzer.First_Column;
      New_Analyzer.Lookahead_Queue        := null;
      New_Analyzer.Lookahead_Head         := null;
      New_Analyzer.Lookahead_Tail         := null;
      New_Analyzer.Lookahead_Count        := 0;
      New_Analyzer.Max_Lookahead          := 0;

      return New_Analyzer;
   end Initialize;

   function Name (Analyzer : in Instance; ID : in Token_ID) return String
   is begin
      return Name (Analyzer.Syntax_List (ID).Token_Handle.all);
   end Name;

   procedure Reset (Analyzer : in out Instance)
   is
      Prev : Token_List_Node_Pointer;
   begin
      Analyzer.Line        := 1;
      Analyzer.Column      := Analyzer.First_Column;
      Analyzer.Lexeme_Head := 1;
      Analyzer.Lexeme_Tail := 0;
      Analyzer.Last_Token_ID  := Analyzer.Default_Token;

      Analyzer.Read_From_Lookahead := True;

      Analyzer.Buffer_Head            := Analyzer.Buffer'First;
      Analyzer.Buffer_Tail            := Analyzer.Buffer'Last;
      Analyzer.Buffer_Size            := 0;
      Analyzer.Buffer_Head_Source_Pos := 1;
      Analyzer.Next_Line              := 1;
      Analyzer.Next_Column            := Analyzer.First_Column;

      loop
         exit when Analyzer.Lookahead_Tail = null;
         Free (Analyzer.Lookahead_Tail.Token_Handle);
         Prev := Analyzer.Lookahead_Tail.Prev;
         Free (Analyzer.Lookahead_Tail);
         Analyzer.Lookahead_Tail := Prev;
      end loop;

      Analyzer.Lookahead_Head  := null;
      Analyzer.Lookahead_Queue := null;

      Analyzer.Lookahead_Count := 0;
      Analyzer.Max_Lookahead   := 0;
   end Reset;

   procedure Set_Text_Feeder (Analyzer : in out Instance; Feeder : in Text_Feeder.Text_Feeder_Ptr) is
   begin
      Analyzer.Feeder      := Feeder;
      Analyzer.Line        := 1;
      Analyzer.Column      := Analyzer.First_Column;
      Analyzer.Next_Line   := 1;
      Analyzer.Next_Column := Analyzer.First_Column;
   end Set_Text_Feeder;

   procedure Set_Syntax (Analyzer : in out Instance; Language_Syntax : in Syntax)
   is begin
      --  This copies the pointers to recognizers, which is why this is
      --  unsafe.
      Analyzer.Syntax_List := Language_Syntax;

      --  If Language_Syntax was created with the Get in this package,
      --  using the default New_Token parameter, the Token IDs are all
      --  Token_ID'First, which is wrong. So fix that now.
      for ID in Syntax'Range loop
         Analyzer.Syntax_List (ID).Token_Handle.ID := ID;
      end loop;
   end Set_Syntax;

   function End_Of_Text (Analyzer : in Instance) return Boolean
   is begin
      return End_Of_Buffered_Text (Analyzer) and Text_Feeder.End_Of_Text (Analyzer.Feeder.all);
   end End_Of_Text;

   function End_Of_Buffered_Text (Analyzer : in Instance) return Boolean
   is begin
      return Analyzer.Buffer_Size = 0 or Analyzer.Buffer_Head = Analyzer.Buffer_Tail;
   end End_Of_Buffered_Text;

   procedure Discard_Buffered_Text (Analyzer : in out Instance)
   is begin
      Analyzer.Buffer_Head := Analyzer.Buffer'First;
      Analyzer.Buffer_Tail := Analyzer.Buffer'Last;
      Analyzer.Buffer_Size := 0;
   end Discard_Buffered_Text;

   procedure Set_Default
     (Analyzer : in out Instance;
      Default  : in     Terminal_ID)
   is begin
      Analyzer.Has_Default   := True;
      Analyzer.Default_Token := Default;
   end Set_Default;

   procedure Unset_Default (Analyzer : in out Instance)
   is begin
      Analyzer.Has_Default := False;
   end Unset_Default;

   overriding procedure Find_Next
     (Analyzer   : in out Instance;
      Look_Ahead : in     Boolean := False)
   is
      EOLs_Found : Integer;

      Matched_Token_ID : Syntax_ID;
      Matched_Length   : Natural;
   begin
      --  We allow Create to changing the token stored in the syntax
      --  list, to avoid another new/free pair.

      if Look_Ahead and Analyzer.Lookahead_Tail = null then
         --  Push the previous token on the lookahead queue tail, so
         --  Push_Back can restore it if necessary. Note that this
         --  copies any user data from the analyzer and recognizer, if
         --  Create stored them in the token.

         Analyzer.Lookahead_Tail := new Token_List_Node'
           (Token_Handle => new Enumerated.Class'
              (Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all),
            Prev         => null,
            Next         => null);
      end if;


      if Look_Ahead or Analyzer.Lookahead_Queue = null then

         if Analyzer.Lookahead_Head = null then
            Analyzer.Read_From_Lookahead := False;

            loop
               Find_Best_Match (Analyzer, Matched_Token_ID, Matched_Length);

               --  If we didn't find a match, its a either syntax error
               --  or a match to the default token.

               if Matched_Length = 0 then
                  if Analyzer.Has_Default then

                     --  Find all the characters that *aren't* part of a
                     --  match
                     Find_Non_Match
                       (Unmatched_Length => Matched_Length,
                        Analyzer         => Analyzer);
                     Matched_Token_ID := Analyzer.Default_Token;

                  else
                     raise Syntax_Error with "Unrecognized character '" & Analyzer.Buffer (Analyzer.Buffer_Head) & "'";
                  end if;
               end if;

               if Analyzer.Syntax_List (Matched_Token_ID).Recognizer.Report then
                  --  Save off the information for the token we found
                  Analyzer.Lexeme_Head := Analyzer.Buffer_Head;
                  Analyzer.Lexeme_Tail := Increment_Buffer_Index
                    (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Head, Matched_Length - 1);

                  Analyzer.Lexeme_Source_Pos := Analyzer.Buffer_Head_Source_Pos;

                  --  This allows the user token to modify
                  --  Syntax_List, as well as cache recognizer and
                  --  analyzer info in the token.
                  Create
                    (Lexeme     => Lexeme (Analyzer),
                     Bounds     => Bounds (Analyzer),
                     Recognizer => Analyzer.Syntax_List (Matched_Token_ID).Recognizer,
                     New_Token  => Analyzer.Syntax_List (Matched_Token_ID).Token_Handle.all);

                  Analyzer.Line   := Analyzer.Next_Line;
                  Analyzer.Column := Analyzer.Next_Column;
               end if;

               EOLs_Found := EOLs_Buffered (Analyzer, Matched_Length);
               Analyzer.Next_Line := Analyzer.Next_Line + EOLs_Found;

               if EOLs_Found = 0 then
                  Analyzer.Next_Column := Analyzer.Next_Column + Matched_Length;
               else
                  Analyzer.Next_Column := Analyzer.First_Column + Characters_After_Last_EOL (Analyzer, Matched_Length);
               end if;

               --  Ditch the last token to make room for more parsing
               Analyzer.Buffer_Head := Increment_Buffer_Index
                 (Analyzer.Max_Buffer_Size, Analyzer.Buffer_Head, Matched_Length);
               Analyzer.Buffer_Size := Analyzer.Buffer_Size - Matched_Length;

               Analyzer.Buffer_Head_Source_Pos := Analyzer.Buffer_Head_Source_Pos + Matched_Length;

               exit when Analyzer.Syntax_List (Matched_Token_ID).Recognizer.Report;

            end loop;

            if Look_Ahead then
               --  Push the matched token on the lookahead queue tail.

               Analyzer.Lookahead_Tail.Next := new Token_List_Node'
                 (Token_Handle => new Enumerated.Class'(Analyzer.Syntax_List (Matched_Token_ID).Token_Handle.all),
                  Prev         => Analyzer.Lookahead_Tail,
                  Next         => null);

               Analyzer.Lookahead_Tail := Analyzer.Lookahead_Tail.Next;

               if Analyzer.Lookahead_Queue = null then
                  --  We want Lookahead_Queue to point to the first
                  --  token read with Look_Ahead True.
                  Analyzer.Lookahead_Queue := Analyzer.Lookahead_Tail;
               end if;

               if Trace_Parse > 0 then
                  Trace_Put ("look ahead " & Token_Image (Analyzer.Last_Token_ID)); Ada.Text_IO.New_Line;
                  Analyzer.Lookahead_Count := Analyzer.Lookahead_Count + 1;
                  if Analyzer.Lookahead_Count > Analyzer.Max_Lookahead then
                     Analyzer.Max_Lookahead := Analyzer.Lookahead_Count;
                     Trace_Put ("max look ahead" & Integer'Image (Analyzer.Max_Lookahead));
                     Ada.Text_IO.New_Line;
                  end if;
               end if;
            end if;
            Analyzer.Last_Token_ID := Matched_Token_ID;
         else
            --  Read from lookahead_head
            Analyzer.Read_From_Lookahead := True;
            Analyzer.Last_Token_ID := Analyzer.Lookahead_Head.Token_Handle.ID;

            Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all := Analyzer.Lookahead_Head.Token_Handle.all;
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Head.Next;

            if Trace_Parse > 0 then
               Trace_Put ("look ahead " &  Name (Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all));
               Ada.Text_IO.New_Line;
            end if;
         end if;
      else
         --  Read the next token from the lookahead queue head

         --  Free the previous token. Note that the queue contains one
         --  token behind lookahead_queue, in case Push_Back needs it.
         --  That's the one we free.
         Free (Analyzer.Lookahead_Queue.Prev.Token_Handle);
         Free (Analyzer.Lookahead_Queue.Prev);

         --  Keep track of Lookahead_Head in case the next call with
         --  Look_Ahead => True happens before the queue is emptied.
         if Analyzer.Lookahead_Queue = Analyzer.Lookahead_Head then
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Queue.Next;
         end if;

         --  Pop the first item off the queue and put it into the syntax list
         Analyzer.Last_Token_ID := Analyzer.Lookahead_Queue.Token_Handle.ID;
         Analyzer.Read_From_Lookahead := True;
         Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all := Analyzer.Lookahead_Queue.Token_Handle.all;
         Analyzer.Lookahead_Queue := Analyzer.Lookahead_Queue.Next;

         if Analyzer.Lookahead_Queue = null then
            Analyzer.Lookahead_Tail := null;
         end if;

         if Trace_Parse > 0 then
            Analyzer.Lookahead_Count := Analyzer.Lookahead_Count - 1;
         end if;
      end if;
   exception
   when E : Syntax_Error =>
      raise Syntax_Error with
        Int_Image (Line (Analyzer)) &
        ":" &
        Int_Image (Column (Analyzer)) &
        " " &
        Ada.Exceptions.Exception_Message (E);
   end Find_Next;

   overriding function Mark_Push_Back (Analyzer : in Instance) return Token.Queue_Mark'Class
   is begin
      return Queue_Mark'
        (Head => Analyzer.Lookahead_Head,
         Tail => Analyzer.Lookahead_Tail);
   end Mark_Push_Back;

   overriding procedure Push_Back (Analyzer : in out Instance; Mark : in Token.Queue_Mark'Class)
   is
      My_Mark  : Queue_Mark renames Queue_Mark (Mark);
      End_Mark : Token_List_Node_Pointer;
   begin
      if Trace_Parse > 0 then
         Trace_Put ("...push back");
      end if;

      --  We must preserve the pushed back items in the queue, in case
      --  another backtrack occurs.

      if My_Mark.Head = null then
         if My_Mark.Tail = null then
            --  Push back all of the queue
            End_Mark := Analyzer.Lookahead_Queue;
         else
            End_Mark := My_Mark.Tail.Next;
         end if;
      else
         End_Mark := My_Mark.Head;
      end if;

      loop
         exit when Analyzer.Lookahead_Head = End_Mark;

         if Analyzer.Lookahead_Head = null then

            Analyzer.Last_Token_ID := Analyzer.Lookahead_Tail.Prev.Token_Handle.ID;
            Analyzer.Read_From_Lookahead := True;

            Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all :=
              Analyzer.Lookahead_Tail.Prev.Token_Handle.all;

            Analyzer.Lookahead_Head := Analyzer.Lookahead_Tail;
         else
            Analyzer.Lookahead_Head := Analyzer.Lookahead_Head.Prev;

            Analyzer.Last_Token_ID := Analyzer.Lookahead_Head.Prev.Token_Handle.ID;
            Analyzer.Read_From_Lookahead := True;

            Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all :=
              Analyzer.Lookahead_Head.Prev.Token_Handle.all;

         end if;

      end loop;

      if Trace_Parse > 0 then
         Ada.Text_IO.Put_Line
           ("; current token " & Name (Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all));
      end if;
   end Push_Back;

   function First_Column (Analyzer : in Instance) return Boolean is
   begin
      return Analyzer.Next_Column = 1;
   end First_Column;

   function Next_Token_Column (Analyzer : in Instance) return Integer is
   begin
      return Analyzer.Next_Column;
   end Next_Token_Column;

   function Line (Analyzer : in Instance) return Natural is
   begin
      return Analyzer.Line;
   end Line;

   function Column (Analyzer : in Instance) return Natural is
   begin
      return Analyzer.Column;
   end Column;

   overriding function Get (Analyzer : in Instance) return OpenToken.Token.Class
   is begin
      return Analyzer.Syntax_List (Analyzer.Last_Token_ID).Token_Handle.all;
   end Get;

   function ID (Analyzer : in Instance) return Terminal_ID is
   begin
      return Analyzer.Last_Token_ID;
   end ID;

   overriding function Lexeme (Analyzer : in Instance) return String
   is begin
      if Analyzer.Read_From_Lookahead then
         raise Programmer_Error;
      end if;

      if Analyzer.Lexeme_Tail = 0 then
         return "";
      end if;

      if Analyzer.Lexeme_Tail < Analyzer.Lexeme_Head then
         return Analyzer.Buffer (Analyzer.Lexeme_Head .. Analyzer.Buffer'Last) &
           Analyzer.Buffer (Analyzer.Buffer'First .. Analyzer.Lexeme_Tail);
      else
         return Analyzer.Buffer (Analyzer.Lexeme_Head .. Analyzer.Lexeme_Tail);
      end if;
   end Lexeme;

   function Get
     (Recognizer : in OpenToken.Recognizer.Class;
      New_Token  : in OpenToken.Token.Enumerated.Class := OpenToken.Token.Enumerated.Get)
     return Recognizable_Token
   is begin
      return
        (Recognizer   => new OpenToken.Recognizer.Class'(Recognizer),
         Token_Handle => new OpenToken.Token.Enumerated.Class'(New_Token));
   end Get;

   function Bounds (Analyzer : in Instance) return Buffer_Range
   is begin
      if Analyzer.Read_From_Lookahead then
         raise Programmer_Error;
      end if;

      return (Analyzer.Lexeme_Source_Pos, Analyzer.Lexeme_Source_Pos + Lexeme (Analyzer)'Length - 1);
   end Bounds;

   overriding function Last_Recognizer (Analyzer : in Instance) return Recognizer_Handle
   is begin
      if Analyzer.Read_From_Lookahead then
         --  Only a problem if the recognizer changes dynamically,
         --  which we allow it to do via Create.
         raise Programmer_Error;
      end if;
      return Analyzer.Syntax_List (Analyzer.Last_Token_ID).Recognizer;
   end Last_Recognizer;

end OpenToken.Token.Enumerated.Analyzer;
