
# Rules file syntax test suite



 Those tests check that the variaton in comment, casing,
 punctuation, etc. do not impact rules understanding.


##  Rules file syntax test suite / Reference file

  Test: Reference file

  This rules file will serve as reference for all folowing tests

  ```
App contains Main
GUI contains Gtk
GUI contains Glib
GUI contains Pango

  ```

  ```
  archicheck -lr reference_rules.txt
  ```

  The reference result is :

  ```
reference_rules.txt:2.1: Component App contains Unit Main
reference_rules.txt:3.1: Component GUI contains Unit Gtk
reference_rules.txt:4.1: Component GUI contains Unit Glib
reference_rules.txt:4.19: Component GUI contains Unit Pango
  ```


 Rules file syntax test suite / Reference file [Successful]

##  Rules file syntax test suite / Casing


  ```
App contains Main
GUI CONTAINS Gtk
GUI coNTains Glib
GUI Contains Pango

  ```


 Rules file syntax test suite / Casing [Successful]

##  Rules file syntax test suite / Spacing and comments


  ```





App contains Main -- final comment

-- comment

   -- Tab and extra spaces :
			GUI       contains 	Gtk
			
GUI contains Glib

-- comment, should not be taken into account :
-- DB contains DB.Query  *********************

-- comment : 
--DB contains DB.IO 
  ```


 Rules file syntax test suite / Spacing and comments [Successful]

##  Rules file syntax test suite / Punctuation and syntaxic sugar

  Rules using syntaxic sugar, such as comma, semicolon, and, dot
  Almost natural english written rules file!

  ```


App contains Main;
GUI contains ATK GIO Gtk, Glib and Pango.

  ```


 Rules file syntax test suite / Punctuation and syntaxic sugar [Empty]
