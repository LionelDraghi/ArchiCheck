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

package List_Image is

   EOL : constant String := ASCII.CR & ASCII.LF;
   -- This should be OK with most systems, but it may be overloaded
   -- in the Style instantiation.

   -- --------------------------------------------------------------------------
   --                               Style
   -- --------------------------------------------------------------------------
   -- This signature package defines the style of the String returned by the
   -- Image function.
   --
   -- Prefix, Postfix and Separator parameters are self explaining.
   -- If Prefix = '(', Postfix = ')', and Separator = ',', Image will be of
   -- this kind : (A,B,C,D)
   -- If all parameters are set to "", the image will be : ABCD
   --
   -- Special Prefix and Postfix are possible for null list, and for list with
   -- a single element.
   -- This is usefull when you want to want "[A,B,C]" as an image, but you don't
   -- want "[]" when the list is empty.
   --
   -- A usefull application of this feature is to have well written comments
   -- regarding singular and plural.
   -- If you want your image to be "A item found" but "A, B, C items found",
   -- just set Postfix to " items found", and Postfix_If_Single to
   -- " item found".
   -- And by the way, if you want the Image to be "No item found" when the
   -- list is emtpy, Prefix_If_Empty and Postfix_If_Empty are here for you.
   --
   -- Last_Separator allows to have this kind of output :
   -- "A, B, C and D"
   --
   -- Note that Separator may be whatever String. You may want to insert an End
   -- of Line sequence to split the list on several line, the EOL String and
   -- parameters are provided for that purpose.

   generic
      Prefix    : String := "";
      Postfix   : String := "";
      Separator : String := "";

      Last_Separator    : String := Separator;
      Prefix_If_Empty   : String := Prefix;
      Postfix_If_Empty  : String := Postfix;
      Prefix_If_Single  : String := Prefix;
      Postfix_If_Single : String := Postfix;

      EOL               : String := List_Image.EOL;

   package Image_Style is end;

   -- --------------------------------------------------------------------------
   --                         Predefined style
   -- --------------------------------------------------------------------------
   --
   -- - Default_Style :
   --   > A, B, C
   --
   -- - English_Style :
   --   > A, B and C
   --
   -- - Bracketed_List_Style :
   --   > [A, B, C]
   --
   -- - Bulleted_List_Style :
   --   > - A
   --   > - B
   --   > - C
   --
   -- - Markdown_Bulleted_List_Style :
   --   Like the bulleted list, but surrounded by
   --   two empty lines (in some Markdown implementation, if the first bullet
   --   is not preceded by an empty line, the list is not recognized)
   --
   -- - HTML_Bulleted_List_Style :
   --   > <ul>
   --   > <li>A</li>
   --   > <li>B</li>
   --   > <li>C</li>
   --   > </ul>
   --   Note : <ul></ul>, an empty list, is recognized by most navigator,
   --          but seems to be illegal html.
   --          No problem here, thanks to _If_Empty parameters nothing will
   --          be generated if the list is emty.
   --
   -- - Markdown_Table_Style :
   --   > | A | B | C |
   --   Note : Markdown don't define tables, but it's a common extension,
   --          like in Github Flavored Markdown for example.
   --
   -- --------------------------------------------------------------------------
   package Default_Style is new Image_Style (Separator => ", ");

   package English_Style is new Image_Style (Separator      => ", ",
                                            Last_Separator => " and ");

   package Bracketed_List_Style is new Image_Style (Prefix    => "[",
                                                   Postfix   => "]",
                                                   Separator => ", ");

   package Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL,
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

   package Markdown_Bulleted_List_Style is new Image_Style
     (Prefix           => EOL & EOL & "- ",
      Separator        => EOL & "- ",
      Postfix          => EOL & EOL,
      Prefix_If_Empty  => EOL,
      Postfix_If_Empty => "");

   package HTML_Bulleted_List_Style is new Image_Style
     (Prefix           => "<ul>"  & EOL & "  <li>",
      Separator        => "</li>" & EOL & "  <li>",
      Postfix          => "</li>" & EOL & "</ul>",
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

   package Markdown_Table_Style is new List_Image.Image_Style
     (Prefix           => "|",
      Separator        => "|",
      Postfix          => "|",
      Prefix_If_Empty  => "",
      Postfix_If_Empty => "");

   -- --------------------------------------------------------------------------
   --                             Cursors
   -- --------------------------------------------------------------------------
   generic
      type Container (<>) is limited private;
      type Cursor is private;
      with function First (Self : Container) return Cursor is <>;
      with function Has_Element (Pos : Cursor) return Boolean is <>;
      with function Next        (Pos : Cursor) return Cursor  is <>;
   package Cursors_Signature is end;

   -- --------------------------------------------------------------------------
   --                       The Image function
   -- --------------------------------------------------------------------------
   generic
      with package Cursors is new Cursors_Signature (<>);
      with function Image (C : Cursors.Cursor) return String is <>;
      with package Style is new Image_Style (<>);
   function Image (Cont : in Cursors.Container) return String;

end List_Image;
