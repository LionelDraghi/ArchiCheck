File: Rules file syntax

Those tests check that the variaton in comment, casing,
punctuation, etc. do not impact rules understanding.

Test: Reference file

This rules file will serve as reference for all folowing tests

(start code)
App contains Main
GUI contains Gtk
GUI contains Glib
GUI contains Pango

(end)

(start code)
archicheck -lr reference_rules.txt
(end)

The reference result is :

(start code)
reference_rules.txt:2.1: Component App contains Unit Main
reference_rules.txt:3.1: Component GUI contains Unit Gtk
reference_rules.txt:4.1: Component GUI contains Unit Glib
reference_rules.txt:4.19: Component GUI contains Unit Pango
(end)

Test: Casing

(start code)
App contains Main
GUI CONTAINS Gtk
GUI coNTains Glib
GUI Contains Pango

(end)

Test: Spacing and comments

(start code)
App contains Main -- final comment

-- comment

   -- Tab and extra spaces :
			GUI       contains 	Gtk
			
GUI contains Glib

-- Not a comment, should be taken into account :
- GUI contains Pango

-- comment, should not be taken into account :
-- DB contains DB.Query  *********************

-- comment : 
--DB contains DB.IO 
(end)

Test: NaturalDocs format

(start code)
-- File: My Rules
-- 
-- My Comments.................
-- ............................
-- 
App contains Main

-- GUI list of component:
- GUI contains Gtk
- GUI contains Glib
- GUI contains Pango

(end)

Test: Punctuation and syntaxic sugar

(start code)
App contains Main;
GUI contains Gtk, Glib and Pango.

(end)

