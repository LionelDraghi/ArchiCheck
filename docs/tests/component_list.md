
# Component definition rules test suite


    Check the component list extraction from various rule's files

##  Component definition rules test suite / One component list

  Running
  > archicheck --list_rules rules.1

  with file rules.1 :

  ```
App contains Main
  ```

  should output :

  ```
rules.1:1.18: Component App contains Unit Main
  ```


 Component definition rules test suite / One component list [Successful]

##  Component definition rules test suite / GUI component contains 3 other components, declared one by one on the rules file

  Running
  > archicheck --list_rules rules.2

  with file rules.2 :

  ```
App contains Main
GUI contains Gtk
GUI contains Glib
GUI contains Pango
  ```

  should output :

  ```
rules.2:2.1: Component App contains Unit Main
rules.2:3.1: Component GUI contains Unit Gtk
rules.2:4.1: Component GUI contains Unit Glib
rules.2:4.19: Component GUI contains Unit Pango
  ```


 Component definition rules test suite / GUI component contains 3 other components, declared one by one on the rules file [Successful]

##  Component definition rules test suite / GUI component contains 3 other components, declared all in one line in the rules file

  Running
  > archicheck --list_rules rules.3

  with file rules.3 :

  ```
App contains Main
GUI contains Gtk and Glib and Pango
  ```

  should output :

  ```
rules.3:2.1: Component App contains Unit Main
rules.3:2.36: Component GUI contains Unit Pango
rules.3:2.36: Component GUI contains Unit Glib
rules.3:2.36: Component GUI contains Unit Gtk
  ```


 Component definition rules test suite / GUI component contains 3 other components, declared all in one line in the rules file [Successful]