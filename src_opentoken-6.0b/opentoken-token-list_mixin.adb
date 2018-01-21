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
with OpenToken.Token.Linked_List;
package body OpenToken.Token.List_Mixin is

   overriding
   function Image (Item : in Instance) return String
   is
      pragma Unreferenced (Item);
   begin
      return "";
   end Image;

   function "**"
     (Element   : access Component_Token'Class;
      Separator : access OpenToken.Token.Class)
     return Instance
   is begin
      return
        (Parent_Token with
         Element     => Component_Handle (Element),
         Separator   => OpenToken.Token.Handle (Separator),
         Lookahead   => Default_Lookahead,
         Initialize  => null,
         Add_Element => null,
         Build       => null);
   end "**";

   function "*"
     (Token       : in Instance;
      Add_Element : in Element_Action)
     return Handle
   is begin
      return New_Instance (Token, Add_Element => Add_Element);
   end "*";

   function "+"
     (Token      : in Handle;
      Initialize : in List_Action)
     return Handle
   is begin
      Token.Initialize := Initialize;
      return Token;
   end "+";

   function "-"
     (Token : in Handle;
      Build : in List_Action)
     return Handle
   is begin
      Token.Build := Build;
      return Token;
   end "-";

   function Get
     (Element     : access Component_Token'Class;
      Separator   : access OpenToken.Token.Class;
      Name        : in     String                := "";
      Lookahead   : in     Integer               := Default_Lookahead;
      Initialize  : in     List_Action           := null;
      Add_Element : in     Element_Action        := null;
      Build       : in     List_Action           := null)
     return Instance
   is
      New_Token : Instance :=
        (Parent_Token with
         Element     => Component_Handle (Element),
         Separator   => OpenToken.Token.Handle (Separator),
         Lookahead   => Lookahead,
         Initialize  => Initialize,
         Add_Element => Add_Element,
         Build       => Build);
   begin
      Set_Name (OpenToken.Token.Instance (New_Token), Name);

      return New_Token;
   end Get;

   function New_Instance
     (Token       : in Instance;
      Name        : in String         := "";
      Lookahead   : in Integer        := Default_Lookahead;
      Initialize  : in List_Action    := null;
      Add_Element : in Element_Action := null;
      Build       : in List_Action    := null)
     return Handle
   is
      New_Token : constant Handle := new Class'(Class (Token));
   begin
      Set_Name (OpenToken.Token.Instance (New_Token.all), Name);

      if Lookahead /= Default_Lookahead then
         New_Token.Lookahead := Lookahead;
      end if;

      if Initialize /= null then
         New_Token.Initialize := Initialize;
      end if;
      if Add_Element /= null then
         New_Token.Add_Element := Add_Element;
      end if;
      if Build /= null then
         New_Token.Build := Build;
      end if;

      return New_Token;
   end New_Instance;

   procedure Set_Lookahead (Token : in out Instance; Lookahead : in Integer)
   is begin
      Token.Lookahead := Lookahead;
   end Set_Lookahead;

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean      := True)
   is
      --  Since this routine can be called recursively, we keep the
      --  working copy on the stack. This provides the operand stack
      --  for the actions.
      Local_Match : Instance := Match.all;
   begin
      if Trace_Parse > 0 then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line
           (" list " & Name_Dispatch (Match) &
              "'(" & Name_Dispatch (Match.Element) & ", " &
              Name_Dispatch (Match.Separator) & ") match " &
              Name_Dispatch (Analyzer.Get));
      end if;

      if Local_Match.Initialize /= null then
         Local_Match.Initialize (Local_Match);
      end if;

      --  Read element, separator until we don't find another
      --  separator. We don't store the parsed tokens; that's up to
      --  the user version of Add_List_Element. Match.Element,
      --  Match.Separator are just patterns, not storage.

      if Actively then
         loop
            OpenToken.Token.Parse (OpenToken.Token.Handle (Local_Match.Element), Analyzer, Actively => True);

            if Local_Match.Add_Element /= null then
               Local_Match.Add_Element (Local_Match, Local_Match.Element.all);
            end if;

            begin
               OpenToken.Token.Parse (Local_Match.Separator, Analyzer, Actively => True);
            exception
            when Parse_Error =>
               exit;
            end;
         end loop;

         if Local_Match.Build /= null then
            Local_Match.Build (Local_Match);
         end if;

         Match.all := Local_Match;
      else
         for J in 1 .. Match.Lookahead loop
            OpenToken.Token.Parse (OpenToken.Token.Handle (Local_Match.Element), Analyzer, Actively => False);

            if Local_Match.Add_Element /= null then
               Local_Match.Add_Element (Local_Match, Local_Match.Element.all);
            end if;

            begin
               OpenToken.Token.Parse (Local_Match.Separator, Analyzer, Actively => False);
            exception
            when Parse_Error =>
               exit;
            end;
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

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is begin
      Linked_List.Add (List, OpenToken.Token.Handle (Token.Element));
   end Expecting;

end OpenToken.Token.List_Mixin;
