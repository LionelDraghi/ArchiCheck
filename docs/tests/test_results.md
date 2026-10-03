## [01_command_line.md](01_command_line.md)  
  - [X] scenario   [Help options](01_command_line.md) pass  

  - [X] scenario   [Version option](01_command_line.md) pass  

  - [X] scenario   [-I option without src dir](01_command_line.md) pass  

  - [X] scenario   [-I option with an unknown dir](01_command_line.md) pass  

  - [X] scenario   [unknown -xyz option](01_command_line.md) pass  

  - [X] scenario   [-I option with... nothing to do](01_command_line.md) pass  

  - [X] scenario   [-lr option without rules file](01_command_line.md) pass  

  - [X] scenario   [Legal line, but no src file in the given (existing) directory](01_command_line.md) pass  

  - [X] scenario   [file given to -I, instead of a directory](01_command_line.md) pass  

  - [X] scenario   [-ld given, but no source found](01_command_line.md) pass  

  - [X] scenario   [src found, but nothing to do with it](01_command_line.md) pass  

  - [X] scenario   [rules file found, but nothing to do with it](01_command_line.md) pass  

  - [X] scenario   [template creation (-ct and --create_template)](01_command_line.md) pass  

  - [X] scenario   [template creation when there's already one](01_command_line.md) pass  

  - [X] scenario   [-ar without rule](01_command_line.md) pass  

## [02_source_list.md](02_source_list.md)  
  ### Feature: --list_files option and sources finding feature  

  - [X] scenario   [Non recursive file identification test](02_source_list.md) pass  


  - [X] scenario   [Recursive file identification test](02_source_list.md) pass  

## [03_dependency_list.md](03_dependency_list.md)  
  ### Feature: Dependencies identification simple test suite  
  - [X] scenario   [Simple test](03_dependency_list.md) pass  

  - [X] scenario   [Source with weird formatted withed unit](03_dependency_list.md) pass  

## [04_component_list.md](04_component_list.md)  
  ### Feature: Component definition rules test suite  
  - [X] scenario   [One component list](04_component_list.md) pass  

  - [X] scenario   [GUI component contains 3 other components, declared one by one on the rules file](04_component_list.md) pass  

  - [X] scenario   [GUI component contains 3 other components, declared all in one line in the rules file](04_component_list.md) pass  

## [05_layer_rule.md](05_layer_rule.md)  
  ### Feature: Layer rules test suite  
  - [X] scenario   [Sanity test, the Batik project architecture](05_layer_rule.md) pass  

  - [X] scenario   [Base normal situation](05_layer_rule.md) pass  

  - [X] scenario   [Illegal upward dependency](05_layer_rule.md) pass  

  - [X] scenario   [Layer bridging](05_layer_rule.md) pass  

  - [X] scenario   [Using a package that is neither in the same layer, nor in the visible layer](05_layer_rule.md) pass  

## [06_child_packages.md](06_child_packages.md)  
  ### Feature: Child packages test suite  

  - [X] scenario   [Rules OK test, no output expected](06_child_packages.md) pass  


  - [X] scenario   [Reverse dependency test](06_child_packages.md) pass  


  - [X] scenario   [Layer bridging test](06_child_packages.md) pass  


  - [X] scenario   [Undescribed dependency test](06_child_packages.md) pass  


  - [X] scenario   [Packages in the same layer may with them self](06_child_packages.md) pass  


  - [X] scenario   [GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg](06_child_packages.md) pass  


  - [X] scenario   [Forbidding Interfaces but allowing Interfaces.C](06_child_packages.md) pass  

## [07_rules_files_syntax.md](07_rules_files_syntax.md)  
  ### Feature: Rules file syntax test suite  
  - [X] scenario   [Reference file](07_rules_files_syntax.md) pass  

  - [X] scenario   [Casing](07_rules_files_syntax.md) pass  

  - [X] scenario   [Spacing and comments](07_rules_files_syntax.md) pass  

## [08_globbing_characters.md](08_globbing_characters.md)  
  ### Feature: Globbing Character test suite  
  - [X] scenario   [rules test](08_globbing_characters.md) pass  

  - [X] scenario   [illegal use of Interfaces from Application_Layer](08_globbing_characters.md) pass  

## [09_gtkada.md](09_gtkada.md)  
  ### Feature: GtkAda test suite  

  - [X] scenario   [File Identification](09_gtkada.md) pass  


  - [X] scenario   [Unit Identification](09_gtkada.md) pass  


  - [X] scenario   [A realistic GtkAda description file](09_gtkada.md) pass  


  - [X] scenario   [Another realistic GtkAda description file](09_gtkada.md) pass  


## Summary : **Fail**

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 43    |
| Empty      | 0     |
| Not Run    | 1     |

