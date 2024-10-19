with Acc.Rules.Lexer;                   use Acc.Rules.Lexer;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Main is
   Rules_File : Ada.Text_IO.File_Type;


   package String_Arrays is new Ada.Containers.Indefinite_Vectors (Positive,
                                                                   String);
   Left_List, Right_List : String_Arrays.Vector;

   type Filling_State is (Not_Filling, Filling_Left_List, Filling_Right_List);
   State : Filling_State;

   use Ada.Strings;

begin
   -- --------------------------------------------------------------------------
   Open (Rules_File, Mode => In_File, Name => Argument (1));

   Analysis : while not End_Of_File (Rules_File) loop
      declare
         Line : aliased constant String := Trim (Get_Line (Rules_File), Side => Both);
         --** déclarer ici un enum type de rule (Rule_Kind)

      begin
         Put_Line ("Analyzing : " & Line);
         State := Not_Filling;
         Acc.Rules.Lexer.Initialize_Cursor;
         Left_List.Clear;
         Right_List.Clear;

         Line_Processing : while Acc.Rules.Lexer.More_Token loop
            declare
               TT  : Token_Type;
               Tok : constant String := Next_Token (Line'Access, TT);
               WTF : exception;

            begin
               case TT is
                  when Keyword =>
                     -- Put_Line ("   Keyword    : " & Tok);
                     if State = Filling_Left_List then
                        State := Filling_Right_List;
                     end if;

                  when Identifier =>
                     -- Put ("   Identifier : " & Tok);
                     if State = Not_Filling then
                        State := Filling_Left_List;
                     end if;
                     -- Put_Line (", " & State'Image);
                     case State is
                        when Filling_Left_List  => Left_List.Append (Tok);
                        when Filling_Right_List => Right_List.Append (Tok);
                        when others => raise WTF;
                     end case;

                  when Comment =>
                     null; -- Put_Line ("   Comment    : " & Tok);

                  when Empty =>
                     null; -- Put_Line ("   Empty line");

               end case;
            end;
         end loop Line_Processing;

         --** afficher seulement si rule_type /= Empty
         Put_Line ("Left_List  = " & Left_List'Image);
         Put_Line ("Right_List = " & Right_List'Image);

      end;

   end loop Analysis;

   Close (Rules_File);
end Main;
