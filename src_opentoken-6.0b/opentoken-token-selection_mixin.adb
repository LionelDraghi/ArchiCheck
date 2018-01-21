-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014 Stephe Leake
--  Copyright (C) 2000 Ted Dennison
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
-------------------------------------------------------------------------------

with Ada.Text_IO;
package body OpenToken.Token.Selection_Mixin is

   overriding
   function Image (Item : in Instance) return String
   is
      pragma Unreferenced (Item);
   begin
      return "";
   end Image;

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean := True)
   is
      use Linked_List;
      I : List_Iterator := First (Match.Members);
   begin
      if Trace_Parse > 0 then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line
           (" selection " & Name_Dispatch (Match) &
              "'(" & Names (Match.Members) & ") match " & Name_Dispatch (Analyzer.Get));
      end if;

      Find_Match :
      loop
         declare
            Mark : Queue_Mark'Class renames Analyzer.Mark_Push_Back;
         begin
            Parse (Token_Handle (I), Analyzer, Actively => False);
            if Actively then
               Analyzer.Push_Back (Mark);
            end if;
            exit Find_Match;

         exception
         when Parse_Error =>
            --  We don't need to call Push_Back here if Next_Token (I)
            --  is null, but we can't tell, and it doesn't hurt.
            Analyzer.Push_Back (Mark);
         end;

         Next_Token (I);

         if I = Null_Iterator then
            if Actively then
               declare
                  Expected : Linked_List.Instance;
               begin
                  Expecting (Match, Expected);
                  raise Parse_Error with "Found " & Name_Dispatch (Analyzer.Get) & "; expected one of " &
                    Token.Linked_List.Names (Expected) & ".";
               end;
            else
               --  The spec says "raise Parse_Error with no message".
               --  If we leave out 'with ""' here, GNAT attaches a
               --  message giving the line number. That's useful in
               --  general, but fails our unit test. So we override it
               --  in this case.
               --
               --  The point of "raise with no message" is to not
               --  spend time computing a nice user message, because
               --  it will be thrown away. I'm not clear whether 'with
               --  ""' or the GNAT default is faster, but it probably
               --  doesn't matter much.
               raise Parse_Error with "";
            end if;
         end if;
      end loop Find_Match;

      if Actively then
         Parse (Token_Handle (I), Analyzer, Actively);
         if Match.Build /= null then
            Match.Build (Match.all, Component_Token'Class (Token_Handle (I).all));
         end if;
      end if;

      if Trace_Parse > 0 then
         Trace_Put ("...succeeded"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
   exception
   when others =>
      if Trace_Parse > 0 then
         Trace_Put ("...failed"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
      raise;
   end Parse;

   function "or"
     (Left  : access Component_Token'Class;
      Right : access Component_Token'Class)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Parent_Token with
         Members => OpenToken.Token.Handle (Left) & OpenToken.Token.Handle (Right),
         Build   => null);
   end "or";

   function "or"
     (Left  : access Component_Token'Class;
      Right : in     Instance) return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Parent_Token with
         Members => OpenToken.Token.Handle (Left) & Right.Members,
         Build   => null);
   end "or";

   function "or"
     (Left  : in     Instance;
      Right : access Component_Token'Class) return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Parent_Token with
         Members => Left.Members & OpenToken.Token.Handle (Right),
         Build   => null);
   end "or";

   function "or"
     (Left  : in Instance;
      Right : in Instance)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Parent_Token with
         Members => Left.Members & Right.Members,
         Build   => null);
   end "or";

   function "+"
     (Selection : in Instance;
      Build     : in Action)
     return Handle
   is begin
      return New_Instance (Selection, Build => Build);
   end "+";

   function "and" (Selection : in Handle; Name : in String) return Handle
   is begin
      Set_Name (Selection.all, Name);
      return Selection;
   end "and";

   function New_Instance
     (Old_Instance : in Instance;
      Name         : in String   := "";
      Build        : in Action   := null)
     return Handle
   is
      New_Token : constant Handle := new Class'(Class (Old_Instance));
   begin
      Set_Name (OpenToken.Token.Instance (New_Token.all), Name);
      if Build /= null then
         New_Token.Build := Build;
      end if;
      return New_Token;
   end New_Instance;

   function Copy (Token : in Handle) return Handle
   is begin
      return Token;
   end Copy;

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is
      use Linked_List;
      I : List_Iterator := First (Token.Members);
   begin
      loop
         exit when I = Null_Iterator;
         Expecting (Token_Handle (I), List);
         Next_Token (I);
      end loop;
   end Expecting;

end OpenToken.Token.Selection_Mixin;
