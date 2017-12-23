
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
reference_rules.txt:2.1: Component App contains unit Main
reference_rules.txt:3.1: Component GUI contains unit Gtk
reference_rules.txt:4.1: Component GUI contains unit Glib
reference_rules.txt:4.19: Component GUI contains unit Pango
```


 Rules file syntax test suite / Reference file [Successful]("tests-status#successful")

##  Rules file syntax test suite / Casing


```
App contains Main
GUI CONTAINS Gtk
GUI coNTains Glib
GUI Contains Pango

```


 Rules file syntax test suite / Casing [Successful]("tests-status#successful")

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


 Rules file syntax test suite / Spacing and comments [Successful]("tests-status#successful")

##  Rules file syntax test suite / Punctuation and syntaxic sugar

  Rules using syntaxic sugar, such as comma, semicolon, and, dot
  Almost natural english written rules file!

```


App contains Main;
GUI contains ATK GIO Gtk, Glib and Pango.

```


 Rules file syntax test suite / Punctuation and syntaxic sugar [Empty]("tests-status#empty")
