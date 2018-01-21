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
package body OpenToken.Token.Sequence_Mixin is

   overriding
   function Image (Item : in Instance) return String
   is
      pragma Unreferenced (Item);
   begin
      return "";
   end Image;

   procedure Set_Lookahead (Token : in out Instance; Lookahead : in Integer)
   is begin
      Token.Lookahead := Lookahead;
   end Set_Lookahead;

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean      := True)
   is
      use Token.Linked_List;

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
           (" sequence " & Name_Dispatch (Match) &
              "'(" & Names (Match.Members) & ") match " & Name_Dispatch (Analyzer.Get));
      end if;

      if Actively then
         while I /= Null_Iterator loop
            Parse (Token_Handle (I), Analyzer, Actively => True);
            Next_Token (I);
         end loop;

         if Match.Build /= null then
            Match.Build (Match.all, Match.Members);
         end if;
      else
         for J in 1 .. Match.Lookahead loop
            Parse (Token_Handle (I), Analyzer, Actively => False);
            Next_Token (I);
            exit when I = Null_Iterator;
         end loop;
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

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Parent_Token with
         Members   => OpenToken.Token.Handle (Left) & OpenToken.Token.Handle (Right),
         Lookahead => Default_Lookahead,
         Build     => null);
   end "&";

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Parent_Token with
         Members   => OpenToken.Token.Handle (Left) & Right.Members,
         Lookahead => Default_Lookahead,
         Build     => null);
   end "&";

   function "&"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Parent_Token with
         Members   => Left.Members & OpenToken.Token.Handle (Right),
         Lookahead => Default_Lookahead,
         Build     => null);
   end "&";

   function "&"
     (Left  : in Instance;
      Right : in Instance)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Parent_Token with
         Members   => Left.Members & Right.Members,
         Lookahead => Default_Lookahead,
         Build     => null);
   end "&";

   function "+"
     (Sequence : in Instance;
      Build    : in Action)
     return Handle
   is begin
      return New_Instance (Sequence, Build => Build);
   end "+";

   function "and" (Sequence : in Handle; Name : in String) return Handle
   is begin
      Set_Name (Sequence.all, Name);
      return Sequence;
   end "and";

   function New_Instance
     (Old_Instance : in Instance;
      Name         : in String   := "";
      Lookahead    : in Integer  := Default_Lookahead;
      Build        : in Action   := null)
     return Handle
   is
      New_Token : constant Handle := new Class'(Class (Old_Instance));
   begin
      Set_Name (OpenToken.Token.Instance (New_Token.all), Name);

      New_Token.Lookahead := Lookahead;

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
   begin
      Add (List, Token_Handle (First (Token.Members)));
   end Expecting;

end OpenToken.Token.Sequence_Mixin;
