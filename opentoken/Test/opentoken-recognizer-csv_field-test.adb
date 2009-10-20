--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
package body OpenToken.Recognizer.CSV_Field.Test is


   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use OpenToken.Recognizer;

      Recognizer : Instance := Get;

      procedure Test
        (Label            : in String;
         Input            : in String;
         Expected_Verdict : in Analysis_Verdict)
      is
         use AUnit.Assertions;

         Verdict : Analysis_Verdict;
      begin
         Clear (Recognizer);

         for I in Input'Range loop
            Analyze (Recognizer, Input (I), Verdict);
         end loop;

         Assert (Verdict = Expected_Verdict, Label & " got " & Analysis_Verdict'Image (Verdict));
      end Test;

   begin
      --  A CSV field is anything except leading or trailing
      --  whitespace and comma.
      Test ("normal", "a", Matches);
      Test ("normal", "abc", Matches);
      Test ("trailing whitespace", "abc ", So_Far_So_Good);
      Test ("leading whitespace", " abc", Failed);
      Test ("internal whitespace", "!@#$ foo", Matches);
      Test ("comma", "a,", Failed);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("OpenToken.Recognizer.CSV_Field.Test");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end OpenToken.Recognizer.CSV_Field.Test;
