-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2008, 2009 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

use type Ada.Strings.Unbounded.Unbounded_String;

-------------------------------------------------------------------------------
--  This package provides types and operatorion for parsing analysis on
--  grammars and LR(k) items.
-------------------------------------------------------------------------------
package body OpenToken.Production.Parser.LRk_Item is

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Item_Lookahead, Item_Lookahead_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Item_Set, Item_Set_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Set_Reference, Set_Reference_Ptr);

   function Compute_Non_Terminals return Token_ID_Set
   is
      Result : Token_ID_Set;
   begin
      Result (Tokenizer.Terminal_ID'First .. Tokenizer.Terminal_ID'Last)               := (others => False);
      Result (Token.Token_ID'Succ (Tokenizer.Terminal_ID'Last) .. Token.Token_ID'Last) := (others => True);
      return Result;
   end Compute_Non_Terminals;

   Non_Terminals : constant Token_ID_Set := Compute_Non_Terminals;

   Line_End : constant String := "" & Ada.Characters.Latin_1.LF;

   function Image (Item : in Token_ID_Set) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Need_Comma then
               Result := Result & ", ";
            end if;
            Result     := Result & Token.Token_ID'Image (I);
            Need_Comma := True;
         end if;
      end loop;
      return To_String (Result);
   end Image;

   ----------------------------------------------------------------------------
   --  For the given nonterminal in the given grammar, find the set of tokens
   --  that its first term could resolve to.
   ----------------------------------------------------------------------------
   function First_Derivations
     (Grammar      : in Production_List.Instance;
      Non_Terminal : in Token.Token_ID;
      Trace        : in Boolean)
     return Token_ID_Set is

      Prod_Iterator  : Production_List.List_Iterator;

      Derived_Token : Token.Token_ID;

      Derivations   : Token_ID_Set := (others => False);
      Added_Tokens  : Token_ID_Set := (others => False);
      Search_Tokens : Token_ID_Set := (others => False);

   begin

      Search_Tokens (Non_Terminal) := True;

      --  As long as we found new non-terminal derivations ...
      while Search_Tokens /= Token_ID_Set'(others => False) loop

         Added_Tokens := (others => False);

         --  ...search all productions for rightmost derivations for
         --  tokens we found last time.
         Prod_Iterator := Production_List.Initial_Iterator (Grammar);
         while not Production_List.Past_Last (Prod_Iterator) loop
            if
              Search_Tokens (Token.ID (Production_List.Get_Production (Prod_Iterator).LHS.all))
            then
               Derived_Token :=
                 Token.ID
                  (Token_List.Token_Handle
                   (Token_List.Initial_Iterator
                    (Production_List.Get_Production (Prod_Iterator).RHS.Tokens)
                    ).all
                   );

               if not Derivations (Derived_Token) then
                  Added_Tokens (Derived_Token) := True;
               end if;
            end if;

            Production_List.Next_Production (Prod_Iterator);
         end loop;

         if Trace then
            Ada.Text_IO.Put_Line (Token.Token_ID'Image (Non_Terminal) & ": adding " & Image (Added_Tokens));
         end if;

         Derivations   := Derivations or Added_Tokens;
         Search_Tokens := Added_Tokens and Non_Terminals;
      end loop;

      return Derivations;
   end First_Derivations;

   function First_Derivations
     (Grammar : in Production_List.Instance;
      Trace   : in Boolean)
     return Derivation_Matrix
   is
      Matrix : Derivation_Matrix;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("First Derivations:");
      end if;

      for NT_Index in Matrix'Range loop
         Matrix (NT_Index) := First_Derivations
           (Grammar      => Grammar,
            Non_Terminal => NT_Index,
            Trace        => Trace);
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Matrix;
   end First_Derivations;

   --------------------------------------------------------------------------
   --  Return an item node made from the given production and iterator
   --  into the production's right hand side.
   --------------------------------------------------------------------------
   function Item_Node_Of
     (Prod      : in OpenToken.Production.Instance;
      Iterator  : in Token_List.List_Iterator;
      Lookahead : in Item_Lookahead_Ptr := null;
      Next      : in Item_Ptr      := null
     ) return Item_Node is

      New_Item      : Item_Node;
      Lookahead_Set : Item_Lookahead_Ptr := Lookahead;
   begin
      New_Item.Prod    := Prod;
      New_Item.Pointer := Iterator;
      New_Item.Next    := Next;

      while Lookahead_Set /= null loop

         New_Item.Lookahead_Set := new Item_Lookahead'(Last       => Lookahead_Set.Last,
                                                       Lookaheads => Lookahead_Set.Lookaheads,
                                                       Next       => New_Item.Lookahead_Set
                                                       );
         Lookahead_Set := Lookahead_Set.Next;
      end loop;

      return New_Item;

   end Item_Node_Of;

   --------------------------------------------------------------------------
   --  Return an item node made from the given production. The pointer
   --  will be set to the start of the right hand side.
   --------------------------------------------------------------------------
   function Item_Node_Of (Prod : in OpenToken.Production.Instance) return Item_Node is
   begin
      return (Prod          => Prod,
              Pointer       => Token_List.Initial_Iterator (Prod.RHS.Tokens),
              Lookahead_Set => null,
              Next          => null
              );
   end Item_Node_Of;

   --------------------------------------------------------------------------
   --  Return an item node made from the given production the iterator
   --  refers to and the given lookahead. The lookaheads will be
   --  copied.
   --------------------------------------------------------------------------
   function Item_Node_Of (Prod      : in Production_List.List_Iterator;
                          Lookahead : in Item_Lookahead_Ptr := null) return Item_Node is
      New_Item : Item_Node;
      Lookahead_Set : Item_Lookahead_Ptr := Lookahead;
   begin
      New_Item.Prod    := Production_List.Get_Production (Prod);
      New_Item.Pointer := Token_List.Initial_Iterator
        (Production_List.Get_Production (Prod).RHS.Tokens);

      while Lookahead_Set /= null loop

         New_Item.Lookahead_Set := new Item_Lookahead'(Last       => Lookahead_Set.Last,
                                                       Lookaheads => Lookahead_Set.Lookaheads,
                                                       Next       => New_Item.Lookahead_Set
                                                       );
         Lookahead_Set := Lookahead_Set.Next;
      end loop;

      return New_Item;
   end Item_Node_Of;

   --------------------------------------------------------------------------
   --  Add the given lookahead to the given lookahead set if it is not
   --  already in there
   --------------------------------------------------------------------------
   procedure Include (Set   : in out Item_Lookahead_Ptr;
                      Value : in     Item_Lookahead;
                      Added :    out Boolean
                     ) is

      Found_Match : Boolean := False;
      Match_Set   : Item_Lookahead_Ptr := Set;
   begin

      --  Look for a lookahead that matches
      while Match_Set /= null loop
         if
           Match_Set.Lookaheads (1 .. Match_Set.Last) =
           Value.Lookaheads (1 .. Value.Last)
         then
            Found_Match := True;
            exit;
         end if;

         Match_Set := Match_Set.Next;
      end loop;

      --  If we didn't find one, add one
      if not Found_Match then
         Set := new Item_Lookahead'(Last       => Value.Last,
                                    Lookaheads => Value.Lookaheads,
                                    Next       => Set
                                    );
      end if;

      Added := not Found_Match;
   end Include;

   --------------------------------------------------------------------------
   --  Add the given lookahead to the given lookahead set if it is not
   --  already in there
   --------------------------------------------------------------------------
   procedure Include (Set   : in out Item_Lookahead_Ptr;
                      Value : in     Item_Lookahead
                     ) is

      Added : Boolean;
   begin
      Include (Set => Set, Value => Value, Added => Added);
   end Include;

   ----------------------------------------------------------------------------
   --  Add an item to the set w/o checking to see if it is in there already.
   ----------------------------------------------------------------------------
   procedure Add (New_Item : in     Item_Node;
                  Target   : in out Item_Set
                 ) is
   begin
      Target.Set := new Item_Node'(Prod          => New_Item.Prod,
                                   Pointer       => New_Item.Pointer,
                                   Lookahead_Set => New_Item.Lookahead_Set,
                                   Next          => Target.Set
                                   );
   end Add;

   --------------------------------------------------------------------------
   --  Return a pointer to the given item in the given set. Null will
   --  be returned if it cannot be found.
   --------------------------------------------------------------------------
   function Find (Left  : in Item_Node;
                  Right : in Item_Set
                 ) return Item_Ptr is

      use type Token_List.List_Iterator;

      Current : Item_Ptr := Right.Set;
   begin
      while Current /= null loop
         if Left.Prod = Current.Prod and Left.Pointer = Current.Pointer then
            return Current;
         end if;
         Current := Current.Next;
      end loop;
      return null;
   end Find;

   ----------------------------------------------------------------------------
   --  Check to see if the given item set is in the set list.
   ----------------------------------------------------------------------------
   function Find (Left  : in Item_Set;
                  Right : in Item_Set_List
                  ) return Item_Set_Ptr is

      Right_Set : Item_Set_Ptr := Right.Head;
      Right_Item : Item_Ptr;

      Left_Size  : Natural := 0;
      Right_Size : Natural;
   begin
      --  Count the number of items in the left set
      Right_Item := Left.Set;
      while Right_Item /= null loop
         Left_Size := Left_Size + 1;

         Right_Item := Right_Item.Next;
      end loop;

      --  Go through the sets in the set list...
      while Right_Set /= null loop

         Right_Item := Right_Set.Set;
         Right_Size := 0;
         while Right_Item /= null loop

            if Find (Right_Item.all, Left) = null then
               exit;
            end if;

            Right_Size := Right_Size + 1;
            Right_Item := Right_Item.Next;
         end loop;

         if Right_Item = null and Left_Size = Right_Size then
            return Right_Set;
         end if;

         Right_Set := Right_Set.Next;
      end loop;

      return null;
   end Find;

   ----------------------------------------------------------------------------
   --  Check to see if the given item set is in the set list.
   ----------------------------------------------------------------------------
   function Is_In (Left  : in Item_Set;
                   Right : in Item_Set_List
                  ) return Boolean is
   begin
      return Find (Left, Right) /= null;
   end Is_In;

   ----------------------------------------------------------------------------
   --  Check to see if the given item set is in the given goto list for the
   --  given symbol.
   ----------------------------------------------------------------------------
   function Is_In (Set_Ptr   : in Item_Set_Ptr;
                   Symbol    : in Token.Token_ID;
                   Goto_List : in Set_Reference_Ptr
                  ) return Boolean is
      Goto_Ptr : Set_Reference_Ptr := Goto_List;
      use type Token.Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Set = Set_Ptr and Goto_Ptr.Symbol = Symbol then
            return True;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return False;
   end Is_In;

   ----------------------------------------------------------------------------
   --  Return the goto set for the given item set on the given token symbol.
   ----------------------------------------------------------------------------
   function Goto_Set (From   : in Item_Set;
                      Symbol : in Token.Token_ID
                     ) return Item_Set_Ptr is

      Goto_Ptr : Set_Reference_Ptr := From.Goto_List;
      use type Token.Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Symbol = Symbol then
            return Goto_Ptr.Set;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return null;
   end Goto_Set;

   --------------------------------------------------------------------------
   --  Merge the new item into an existing item set. The existing set
   --  will take over control of the dynamicly allocated lr(k)
   --  components. If the existing set already contains the new item,
   --  it will not be put in again. In this case, this routine
   --  automaticly deallocates the components of New_Item that were
   --  created within this package.
   --------------------------------------------------------------------------
   procedure Merge (New_Item     : in out Item_Node;
                    Existing_Set : in out Item_Set
                   ) is
      Occurrance : constant Item_Ptr :=
        Find (Left => New_Item, Right => Existing_Set);

      Source_Lookahead      : Item_Lookahead_Ptr;
      Previous_Lookahead    : Item_Lookahead_Ptr;
      Destination_Lookahead : Item_Lookahead_Ptr;
      Found_Match : Boolean;

   begin
      if Occurrance = null then
         Existing_Set.Set :=
           new Item_Node'(Prod          => New_Item.Prod,
                          Pointer       => New_Item.Pointer,
                          Lookahead_Set => New_Item.Lookahead_Set,
                          Next          => Existing_Set.Set
                          );
      else
         --  Merge their lookaheads.
         Source_Lookahead := New_Item.Lookahead_Set;
         while Source_Lookahead /= null loop
            Destination_Lookahead := Occurrance.Lookahead_Set;

            Found_Match := False;
            while Destination_Lookahead /= null loop

               if
                 Destination_Lookahead.Lookaheads (1 .. Destination_Lookahead.Last) =
                 Source_Lookahead.Lookaheads (1 .. Source_Lookahead.Last)
               then
                  Found_Match := True;
                  exit;
               end if;

               Destination_Lookahead := Destination_Lookahead.Next;
            end loop;

            if not Found_Match then
               if Previous_Lookahead = null then
                  New_Item.Lookahead_Set := Source_Lookahead.Next;
               else
                  Previous_Lookahead.Next := Source_Lookahead.Next;
               end if;

               Source_Lookahead.Next := Occurrance.Lookahead_Set;
               Occurrance.Lookahead_Set := Source_Lookahead;

               if Previous_Lookahead = null then
                  Source_Lookahead := New_Item.Lookahead_Set;
               else
                  Source_Lookahead := Previous_Lookahead.Next;
               end if;
            else
               Previous_Lookahead := Source_Lookahead;
               Source_Lookahead := Source_Lookahead.Next;
            end if;

         end loop;

         Free (New_Item);
      end if;
   end Merge;

   ----------------------------------------------------------------------------
   --  Return the closure of the given set of items over the given grammar.
   ----------------------------------------------------------------------------
   function Closure (Set     : in Item_Set;
                     First   : in Derivation_Matrix;
                     Grammar : in Production_List.Instance
                    ) return Item_Set is

      use type Token.Token_ID;
      use type Token.Handle;

      Item      : Item_Ptr := Set.Set;
      Lookahead : Item_Lookahead_Ptr;

      Current : Item_Ptr;
      Start   : Item_Ptr;
      Finish  : Item_Ptr := null;

      Nonterminal_ID : Token.Token_ID;
      Next_Symbol    : Token_List.List_Iterator;
      Production_Iterator : Production_List.List_Iterator;

      Result : Item_Set;

      Merge_From : Item_Node;
   begin
      --  Put copies of everything in the given item in the closure
      while Item /= null loop
         Result.Set :=
           new Item_Node'
           (Prod          => Item.Prod,
            Pointer       => Item.Pointer,
            Lookahead_Set => null,
            Next          => Result.Set);

         Lookahead := Item.Lookahead_Set;
         while Lookahead /= null loop
            Result.Set.Lookahead_Set :=
              new Item_Lookahead'(Last       => Lookahead.Last,
                                  Lookaheads => Lookahead.Lookaheads,
                                  Next       => Result.Set.Lookahead_Set);

            Lookahead := Lookahead.Next;
         end loop;

         Item := Item.Next;
      end loop;

      --  Loop through all the items in the set
      Current := Result.Set;
      Start   := Result.Set;
      loop

         --  If the item after the pointer is a nonterminal, find its
         --  productions and place them in the set.
         if Token_List.Token_Handle (Current.Pointer) /= null and then
           Token.ID (Token_List.Token_Handle (Current.Pointer).all) not in Tokenizer.Terminal_ID
         then
            Nonterminal_ID := Token.ID (Token_List.Token_Handle (Current.Pointer).all);

            Next_Symbol := Current.Pointer;
            Token_List.Next_Token (Next_Symbol);

            --  Loop through all the productions
            Production_Iterator := Production_List.Initial_Iterator (Grammar);
            while not Production_List.Past_Last (Production_Iterator) loop
               if
                 Token.ID (Production_List.Get_Production (Production_Iterator).LHS.all) =
                 Nonterminal_ID
               then

                  --  If the pointer in the initial production was at
                  --  its end, we get its lookaheads.
                  if Token_List.Token_Handle (Next_Symbol) = null then
                     Merge_From :=
                       Item_Node_Of (Prod      => Production_Iterator,
                                     Lookahead => Current.Lookahead_Set
                                     );
                     Merge
                       (New_Item     => Merge_From,
                        Existing_Set => Result
                        );

                  elsif Token.ID (Token_List.Token_Handle (Next_Symbol).all) in Tokenizer.Terminal_ID then

                     Merge_From :=
                       Item_Node_Of (Prod      => Production_Iterator,
                                     Lookahead => new Item_Lookahead'
                                     (Last       => 1,
                                      Lookaheads => (1 => Token.ID (Token_List.Token_Handle (Next_Symbol).all)),
                                      Next       => null
                                      )
                                     );
                     Merge
                       (New_Item     => Merge_From,
                        Existing_Set => Result
                        );
                  else

                     --  Loop through all the terminal IDs
                     for Terminal in Tokenizer.Terminal_ID loop

                        if First (Token.ID (Token_List.Token_Handle (Next_Symbol).all)) (Terminal) then
                           Merge_From :=
                             Item_Node_Of (Prod      => Production_Iterator,
                                           Lookahead => new Item_Lookahead'
                                           (Last       => 1,
                                            Lookaheads => (1 => Terminal),
                                            Next       => null
                                            )
                                           );
                           Merge
                             (New_Item     => Merge_From,
                              Existing_Set => Result
                              );
                        end if;
                     end loop;

                  end if; -- pointer is at last token on RHS, or terminal, or non-terminal
               end if; -- we found a production for the non-terminal

               Production_List.Next_Production (Production_Iterator);
            end loop;
         end if; -- pointer is is at non-terminal

         --  When we get to the end of the set, see if we added any.
         --  If so, just check through the added ones next time
         --  around.
         if Current.Next = Finish then
            exit when Result.Set = Start;

            Finish  := Start;
            Start   := Result.Set;
            Current := Result.Set;

         else
            Current := Current.Next;
         end if;

      end loop;

      return Result;
   end Closure;

   function Goto_Transitions
     (Kernel       : in Item_Set;
      Symbol       : in Token.Token_ID;
      First_Tokens : in Derivation_Matrix;
      Grammar      : in Production_List.Instance;
      Trace        : in Boolean)
     return Item_Set
   is

      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;

      Item       : Item_Ptr := Kernel.Set;
      Pointer    : Token_List.List_Iterator;
      Prod       : OpenToken.Production.Instance;
      Prod_Index : Production_List.List_Iterator;

      Item_ID    : Token.Token_ID;
   begin

      while Item /= null loop

         --  Check kernel items to see if the symbol appears before any of their pointers
         Pointer := Item.Pointer;
         if
           Token_List.Token_Handle (Pointer) /= null and then
           Token.ID (Token_List.Token_Handle (Pointer).all) = Symbol
         then
            Token_List.Next_Token (Pointer);
            Goto_Set.Set := new Item_Node'
              (Item_Node_Of
                 (Prod      => Item.Prod,
                  Iterator  => Pointer,
                  Lookahead => Item.Lookahead_Set,
                  Next      => Goto_Set.Set));

            if Trace then
               Ada.Text_IO.Put_Line
                 ("... Symbol " & Token.Token_ID'Image (Symbol) & " is before pointer");
            end if;
         end if;

         --  Check kernel items to see if symbol is in first tokens
         --  for the item's next token.
         if
           Token_List.Token_Handle (Item.Pointer) /= null and then
           Token.ID (Token_List.Token_Handle (Item.Pointer).all) not in Tokenizer.Terminal_ID and then
           First_Tokens (Token.ID (Token_List.Token_Handle (Item.Pointer).all))(Symbol)
         then
            if Trace then
               Ada.Text_IO.Put_Line
                 ("... Symbol " & Token.Token_ID'Image (Symbol) & " is produced by first token after pointer");
            end if;

            --  Find the production(s) that create that symbol and put
            --  them in
            Prod_Index := Production_List.Initial_Iterator (Grammar);
            while not Production_List.Past_Last (Prod_Index) loop

               Prod := Production_List.Get_Production (Prod_Index);
               Pointer := Token_List.Initial_Iterator (Prod.RHS.Tokens);

               --  if the left is a first token for the item's LHS and
               --  the first token on the RHS matches the requested
               --  symbol, put it in with the pointer after the
               --  symbol.
               Item_ID := Token.ID (Token_List.Token_Handle (Item.Pointer).all);
               if
                 Item_ID not in Tokenizer.Terminal_ID and then
                 ((Item_ID = Token.ID (Prod.LHS.all) or First_Tokens (Item_ID)(Token.ID (Prod.LHS.all))) and
                  Token_List.Token_Handle (Pointer) /= null) and then
                 Token.ID (Token_List.Token_Handle (Pointer).all) = Symbol
               then

                  Token_List.Next_Token (Pointer);
                  declare
                     New_Item : constant Item_Node := Item_Node_Of
                       (Prod     => Prod,
                        Iterator => Pointer,
                        Next     => Goto_Set.Set);
                  begin
                     if null = Find (New_Item, Goto_Set) then
                        Goto_Set.Set := new Item_Node'(New_Item);
                     else
                        if Trace then
                           Ada.Text_IO.Put_Line ("... already in goto set");
                        end if;
                     end if;
                  end;
               end if;

               Production_List.Next_Production (Prod_Index);
            end loop;
         end if;

         Item := Item.Next;
      end loop;

      return Goto_Set;
   end Goto_Transitions;

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Node) is
      Lookahead : Item_Lookahead_Ptr := Subject.Lookahead_Set;
   begin

      --  Free the Lookaheads
      while Lookahead /= null loop
         Subject.Lookahead_Set := Lookahead.Next;
         Free (Lookahead);
         Lookahead := Subject.Lookahead_Set;
      end loop;

   end Free;

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item set.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Set) is
      Item     : Item_Ptr          := Subject.Set;
      Goto_Set : Set_Reference_Ptr := Subject.Goto_List;
   begin
      --  Free the item list
      while Item /= null loop
         Subject.Set := Item.Next;
         Free (Item.all);
         Free (Item);
         Item := Subject.Set;
      end loop;

      --  Free the Goto list
      while Goto_Set /= null loop
         Subject.Goto_List := Goto_Set.Next;
         Free (Goto_Set);
         Goto_Set := Subject.Goto_List;
      end loop;

   end Free;

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item set
   --  list.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Set_List) is
      Set : Item_Set_Ptr := Subject.Head;
   begin
      while Set /= null loop
         Subject.Head := Set.Next;

         Free (Set.all);
         Free (Set);

         Set := Subject.Head;
      end loop;
   end Free;

   function LR0_Kernels
     (Grammar      : in Production_List.Instance;
      First_Tokens : in Derivation_Matrix;
      Trace        : in Boolean)
     return Item_Set_List
   is

      --  Initialize it with the first production with the pointer on
      --  the first position.
      Kernel_List        : Item_Set_List :=
        (Head       => new Item_Set'
           (Set     => new Item_Node'
              (Item_Node_Of
                 (Production_List.Get_Production
                    (Production_List.Initial_Iterator (Grammar)))),
          Goto_List => null,
          Index     => 1,
          Next      => null),
         Size       => 1);

      New_Items_To_Check   : Boolean      := True;
      Previous_Kernel_Head : Item_Set_Ptr := null;
      Checking_Set         : Item_Set_Ptr;
      Old_Items            : Item_Set_Ptr := null;
      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;
         Old_Items            := Previous_Kernel_Head;
         Previous_Kernel_Head := Kernel_List.Head;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= Old_Items loop
            if Trace then
               Ada.Text_IO.Put_Line ("Checking Kernel" & Integer'Image (Checking_Set.Index));
            end if;

            for Symbol in Token.Token_ID loop

               --  Get the goto items for this kernel and symbol
               New_Items := Goto_Transitions
                 (Kernel       => Checking_Set.all,
                  Symbol       => Symbol,
                  First_Tokens => First_Tokens,
                  Grammar      => Grammar,
                  Trace        => Trace);

               --  See if any of the item sets need to be added to our list
               if New_Items.Set /= null then
                  if Trace then
                     Print_Item_Set (New_Items);
                  end if;

                  New_Items_Set := Find (New_Items, Kernel_List);
                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next := Kernel_List.Head;
                     New_Items.Index := Kernel_List.Size + 1;

                     if Trace then
                        Ada.Text_IO.Put_Line ("... adding new kernel" & Integer'Image (New_Items.Index));
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Checking_Set.Goto_List := new Set_Reference'
                       (Set    => Kernel_List.Head,
                        Symbol => Symbol,
                        Next   => Checking_Set.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Set_Ptr   => New_Items_Set,
                        Symbol    => Symbol,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put_Line ("... adding goto" & Integer'Image (New_Items_Set.Index));
                        end if;

                        Checking_Set.Goto_List := new Set_Reference'
                          (Set    => New_Items_Set,
                           Symbol => Symbol,
                           Next   => Checking_Set.Goto_List);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            Checking_Set := Checking_Set.Next;
         end loop;

      end loop;

      return Kernel_List;
   end LR0_Kernels;

   function Token_Name (Subject : in Token.Handle) return String is
   begin
      return Token.Token_ID'Image (Token.ID (Subject.all));
   end Token_Name;
   function Token_Name (Subject : in Nonterminal.Handle) return String is
   begin
      return Token.Token_ID'Image (Token.ID (Subject.all));
   end Token_Name;
   function Token_Name (Subject : in Token.Token_ID) return String is
   begin
      return Token.Token_ID'Image (Subject);
   end Token_Name;

   function Print (Item : in Item_Lookahead) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if Item.Last = 0 then
         return "#";
      else
         for Index in 1 .. Item.Last loop
            if Index > 1 then
               Result := Result & " ";
            end if;
            Result := Result & Token_Name (Item.Lookaheads (Index));
         end loop;
      end if;

      return To_String (Result);
   end Print;

   function Print (Item : in Item_Lookahead_Ptr) return String
   is
      use Ada.Strings.Unbounded;
      Lookahead : Item_Lookahead_Ptr := Item;
      Result    : Unbounded_String := Null_Unbounded_String;
   begin
      if Lookahead = null then
         return "";
      else
         Result := Result & ", ";

         loop
            Result := Result & Print (Lookahead.all);

            Lookahead := Lookahead.Next;

            exit when Lookahead = null;

            Result := Result & "/";
         end loop;

         return To_String (Result);
      end if;
   end Print;

   ----------------------------------------------------------------------------
   --  Print out the given item. This routine is included as a debugging aid.
   ----------------------------------------------------------------------------
   function Print_Item (Item : in Item_Node) return String is
      Token_Index : Token_List.List_Iterator;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("   ") &
        Token_Name (Item.Prod.LHS) &
        "(" & Ada.Tags.Expanded_Name (Item.Prod.LHS.all'Tag) & ")" &
        " <=";

      use type Token_List.List_Iterator;
      use type Token.Handle;

   begin
      Token_Index := Token_List.Initial_Iterator (Item.Prod.RHS.Tokens);

      while Token_List.Token_Handle (Token_Index) /= null loop
         if Token_Index = Item.Pointer then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Token_Name (Token_List.Token_Handle (Token_Index));
         Token_List.Next_Token (Token_Index);
      end loop;
      if Token_List.Token_Handle (Item.Pointer) = null then
         Result := Result & " ^";
      end if;

      Result := Result & Print (Item.Lookahead_Set);
      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Item;

   procedure Print_Item (Item : in Item_Node) is
   begin
      Ada.Text_IO.Put (Print_Item (Item));
   end Print_Item;

   ----------------------------------------------------------------------------
   --  Print out the given list of set references.
   ----------------------------------------------------------------------------
   function Image_Set_Reference_List (Reference_List : in Set_Reference_Ptr) return String
   is
      use Ada.Strings.Unbounded;

      Reference : Set_Reference_Ptr := Reference_List;

      Result : Unbounded_String := Null_Unbounded_String;

   begin
      while Reference /= null loop
         Result := Result &
           "      on " & Token_Name (Reference.Symbol) & " => Set" & Natural'Image (Reference.Set.Index) & Line_End;

         Reference := Reference.Next;
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image_Set_Reference_List;

   function Image (Items : in Item_Set) return String is
      Item   : Item_Ptr := Items.Set;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("Set") &
        Natural'Image (Items.Index) & ":";

      Need_New_Line : Boolean := False;
   begin

      while Item /= null loop

         if Need_New_Line then
            Result := Result & Line_End;
         else
            Need_New_Line := True;
         end if;

         Result := Result & Print_Item (Item.all);

         Item := Item.Next;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   procedure Print_Item_Set (Items : in Item_Set) is
   begin
      Ada.Text_IO.Put_Line (Image (Items));
   end Print_Item_Set;

   ----------------------------------------------------------------------------
   --  Print out all the item sets in the given list. This routine is included
   --  as a debugging aid.
   ----------------------------------------------------------------------------
   function Print_Item_Set_List (Items : in Item_Set_List) return String is
      Set : Item_Set_Ptr := Items.Head;
      Item_Count : Natural := 0;
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Result := Ada.Strings.Unbounded.To_Unbounded_String
        ("Number of Kernel Sets =" & Integer'Image (Items.Size) & Line_End);

      while Set /= null loop
         Result := Result & Image (Set.all) &
           "   Set Goto Transitions:" & Line_End & Image_Set_Reference_List (Set.Goto_List);


         Set := Set.Next;
         Item_Count := Item_Count + 1;
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end Print_Item_Set_List;

   procedure Print_Item_Set_List (Items : in Item_Set_List) is
   begin
      Ada.Text_IO.Put_Line (Print_Item_Set_List (Items));
   end Print_Item_Set_List;

end OpenToken.Production.Parser.LRk_Item;

