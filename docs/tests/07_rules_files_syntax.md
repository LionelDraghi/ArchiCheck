## Feature: Rules file syntax test suite

Those tests check that the variation in comment, casing, punctuation, etc. do not impact rules understanding.  


### Scenario: Reference file

- Given the file `rules1.txt`
~~~
-- Definitions from Grammar declaration in the Acc.Rules.Parser body

-- 1. Component_Declaration, with a single Unit and a Unit_List
App contains Main
GUI contains Gtk, Glib and Pango

-- 2. Layer_Declaration
Gtk is a layer over GLib

-- 3. Use_Declaration, with a single Unit and a Unit_List
Pango may use GLib
Gtk   may use GLib and Interfaces.C

-- 4. Restricted_Use_Declaration, with a single Unit and a Unit_List
Only GLib may use Interfaces.C
Only Gio  may use Interfaces.C and System

-- 5. Forbidden_Use_Declaration
System use is forbidden

-- 6. Allowed_Use_Declaration
Ada use is allowed
~~~

- When I run `./acc -lr rules1.txt` or `./acc --list_rules rules1.txt`  

- then I get
```  
rules1.txt:4: Component App contains unit Main
rules1.txt:5: Component GUI contains unit Gtk, Glib and Pango
rules1.txt:8: Layer Gtk is over layer GLib
rules1.txt:11: Pango may use GLib
rules1.txt:12: Gtk may use GLib
rules1.txt:12: Gtk may use Interfaces.C
rules1.txt:15: Only GLib may use Interfaces.C
rules1.txt:16: Only Gio may use Interfaces.C
rules1.txt:16: Only Gio may use System
rules1.txt:19: Use of System is forbidden
rules1.txt:22: Use of Ada allowed 
```  

### Scenario: Casing

- Given the file `rules2.txt`
```  
-- Definitions from Grammar declaration in the Acc.Rules.Parser body

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

- When I run `./acc -lr rules2.txt`  

- then I get
```  
rules2.txt:4: Component App contains unit Main
rules2.txt:5: Component GUI contains unit Gtk, Glib and Pango
rules2.txt:8: Layer Gtk is over layer GLib
rules2.txt:11: Pango may use GLib
rules2.txt:14: Only GLib may use Interfaces.C
rules2.txt:17: Use of System is forbidden
rules2.txt:20: Use of Ada allowed 
```  

### Scenario: Spacing and comments

- Given the file `rules3.txt`
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

- When I run `./acc -lr rules3.txt`  

- then I get
```  
rules3.txt:4: Component App contains unit Main
rules3.txt:9: Component GUI contains unit Gtk
rules3.txt:11: Component GUI contains unit Glib
```  

### Scenario: Punctuation and syntaxic sugar @WIP

Rules using syntaxic sugar, such as comma, semicolon, and, dot  
Almost natural english written rules file!  

NB: pas encore mur pour ce test (bug du blanc comme séparateur) !!

- Given the file `rules4.txt`
```  
App contains Main;
GUI contains ATK GIO Gtk, Glib and Pango.
```  

- When I run `./acc -lr rules4.txt`  

- then I get
```  
rules2.txt:5: Component App contains unit Main
rules2.txt:8: Component GUI contains unit Gtk, Glib and Pango
```

