
# Rules file syntax test suite



 Those tests check that the variaton in comment, casing,
 punctuation, etc. do not impact rules understanding.


##  Rules file syntax test suite / Reference file

  Test: Reference file

```
rules1.txt:5: Component App contains unit Main
rules1.txt:8: Component GUI contains unit Gtk, Glib and Pango
rules1.txt:11: Layer Gtk is over layer GLib
rules1.txt:12: Pango may use GLib
rules1.txt:15: Gtk may use GLib
rules1.txt:15: Gtk may use Interfaces.C
rules1.txt:16: Only GLib may use Interfaces.C
rules1.txt:19: Only Gio may use Interfaces.C
rules1.txt:19: Only Gio may use System
rules1.txt:22: Use of System is forbidden
rules1.txt:22: Use of Ada allowed 
```

  > archicheck --list_rules expected_lc1.txt

  The reference result is :

```
reference_rules.txt:5: Component App contains unit Main
reference_rules.txt:8: Component GUI contains unit Gtk, Glib and Pango
reference_rules.txt:11: Layer Gtk is over layer GLib
reference_rules.txt:14: Pango may use GLib
reference_rules.txt:17: Only GLib may use Interfaces.C
reference_rules.txt:20: Use of System is forbidden
reference_rules.txt:20: Use of Ada allowed 
```


Rules file syntax test suite / Reference file [Successful](tests_status.md#successful)

##  Rules file syntax test suite / Casing


```
-- Definitions from Grammar declaration in the Archicheck.Rules.Parser body

-- 1. Component_Declaration, with a single Unit and a Unit_List
App coNTains Main
GUI contains Gtk, Glib and Pango

-- 2. Layer_Declaration
Gtk Is A layer over GLib

-- 3. Use_Declaration
Pango MAY use GLib

-- 4. Restricted_Use_Declaration
Only GLib may uSe Interfaces.C

-- 5. Forbidden_Use_Declaration
System use is forbidDen

-- 6. Allowed_Use_Declaration
Ada use is ALLOWED
```


Rules file syntax test suite / Casing [Successful](tests_status.md#successful)

##  Rules file syntax test suite / Spacing and comments


```



App contains Main -- final comment

-- comment

   -- Tab and extra spaces :
			GUI       contains 	Gtk
			
GUI contains Glib

// comment, should not be taken into account :
// DB contains DB.Query  *********************

# comment : 
##DB contains DB.IO 
```


Rules file syntax test suite / Spacing and comments [Successful](tests_status.md#successful)

##  Rules file syntax test suite / Punctuation and syntaxic sugar

  Rules using syntaxic sugar, such as comma, semicolon, and, dot
  Almost natural english written rules file!

```


App contains Main;
GUI contains ATK GIO Gtk, Glib and Pango.

```


Rules file syntax test suite / Punctuation and syntaxic sugar [Empty](tests_status.md#empty)
