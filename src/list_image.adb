-- -----------------------------------------------------------------------------
-- Copyright 2018 Lionel Draghi
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------
-- This file is part of the List_Image project
-- available here https://github.com/LionelDraghi/List_Image
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body List_Image is

   function Image (Cont : in Container) return String is
      I      : constant Iterator_If.Forward_Iterator'Class := Iterator (Cont);
      C1, C2 : Cursor;

      use Ada.Strings.Unbounded;
      Tmp    : Unbounded_String;

   begin
      C1 := Iterator_If.First (I);

      if not Iterator_If.Has_Element (C1) then
         -- empty data structure
         return Style.Prefix_If_Empty & Style.Postfix_If_Empty;

      else
         -- before using the first list item, we need to know if there is
         -- another one.
         C2 := Iterator_If.Next (I, C1);

         if not Iterator_If.Has_Element (C2) then
            -- single item list
            return Style.Prefix_If_Single & Image (C1)
              & Style.Postfix_If_Single;

         else
            -- at least two item in the list
            Tmp := To_Unbounded_String (Style.Prefix)
              & To_Unbounded_String (Image (C1));
            loop
               C1 := C2;
               C2 := Iterator_If.Next (I, C2);
               if Iterator_If.Has_Element (C2) then
                  -- C1 do not yet point the last item
                  Tmp := Tmp & Style.Separator & To_Unbounded_String (Image (C1));

               else
                  -- C1 point the last item
                  Tmp := Tmp & Style.Last_Separator & To_Unbounded_String (Image (C1));
                  exit;

               end if;

            end loop;

         end if;

      end if;
      return (To_String (Tmp & Style.Postfix));

   end Image;

end List_Image;
