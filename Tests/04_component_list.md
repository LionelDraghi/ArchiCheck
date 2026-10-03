
# Feature: Component definition rules test suite

Check the component list extraction from various rule's files  

## Scenario: One component list

- Given the file `rules.1`
```
App contains Main
```

- when I run `./acc --list_rules rules.1`

- Then I get 
```  
rules.1:1: Component App contains unit Main
```  

## Scenario: GUI component contains 3 other components, declared one by one on the rules file

- Given the file `rules.2`
```  
App contains Main
GUI contains Gtk
GUI contains Glib
GUI contains Pango
```  

- when I run `./acc --list_rules rules.2`

- Then I get 
```  
rules.2:2: Component App contains unit Main
rules.2:3: Component GUI contains unit Gtk
rules.2:4: Component GUI contains unit Glib
rules.2:4: Component GUI contains unit Pango
```  

## Scenario: GUI component contains 3 other components, declared all in one line in the rules file

- Given the file `rules.3`
```  
App contains Main
GUI contains Gtk and Glib and Pango
```  

- when I run `./acc --list_rules rules.3`

- Then I get 
```  
rules.3:2: Component App contains unit Main
rules.3:2: Component GUI contains unit Gtk, Glib and Pango
```  
