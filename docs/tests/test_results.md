
# Document: [01_command_line.md](01_command_line.md)  
   ### Scenario: [Help options](01_command_line.md): 
   - OK : When I successfully run `./acc -h`,   
   - OK : Then output is  
   - [X] scenario   [Help options](01_command_line.md) pass  

   ### Scenario: [Version option](01_command_line.md): 
   - OK : When I run `./acc --version`  
   - OK : Then I get  
   - [X] scenario   [Version option](01_command_line.md) pass  

   ### Scenario: [-I option without src dir](01_command_line.md): 
   - OK : When running `./acc -I`,  
   - OK : Then I get  
   - [X] scenario   [-I option without src dir](01_command_line.md) pass  

   ### Scenario: [-I option with an unknown dir](01_command_line.md): 
   - OK : When running `./acc -I qsdqjh`    
   - OK : Then I get  
   - [X] scenario   [-I option with an unknown dir](01_command_line.md) pass  

   ### Scenario: [unknown -xyz option](01_command_line.md): 
   - OK : When running `./acc -xzy`    
   - OK : Then I get   
   - [X] scenario   [unknown -xyz option](01_command_line.md) pass  

   ### Scenario: [-I option with... nothing to do](01_command_line.md): 
   - OK : Given the directory `dir6`      
   - OK : Given the file `dir6/src.adb`  
   - OK : When I run `./acc -I dir6`      
   - OK : Then I get   
   - [X] scenario   [-I option with... nothing to do](01_command_line.md) pass  

   ### Scenario: [-lr option without rules file](01_command_line.md): 
   - OK : When I run `./acc -lr  `  
   - OK : Then I get   
   - [X] scenario   [-lr option without rules file](01_command_line.md) pass  

   ### Scenario: [Legal line, but no src file in the given (existing) directory](01_command_line.md): 
   - OK : Given there is no `dir9` directory  
   - OK : Given the new directory `dir9`  
   - OK : When I run `./acc -lf -I dir9`    
   - OK : Then I get `Warning : Cannot list files, no sources found to analyze`  
   - [X] scenario   [Legal line, but no src file in the given (existing) directory](01_command_line.md) pass  

   ### Scenario: [file given to -I, instead of a directory](01_command_line.md): 
   - OK : Given new file `rules.txt` containing `Interfaces use is forbidden`  
   - OK : Given file `src.adb`  
   - OK : When I run `./acc rules.txt -I src.adb`  
   - OK : Then I get `Error : src.adb is not a directory`    
   - [X] scenario   [file given to -I, instead of a directory](01_command_line.md) pass  

   ### Scenario: [-ld given, but no source found](01_command_line.md): 
   - OK : Given new file `rules.txt` containing `Interfaces use is forbidden`  
   - OK : Given new directory `dir11`  
   - OK : When I run `./acc rules.txt -ld -I dir11`    
   - OK : Then I get   
   - [X] scenario   [-ld given, but no source found](01_command_line.md) pass  

   ### Scenario: [src found, but nothing to do with it](01_command_line.md): 
   - OK : Given directory `dir12`  
   - OK : Given file `dir12/src.adb`  
   - OK : When I run `./acc -I dir12`      
   - OK : Then I get   
   - [X] scenario   [src found, but nothing to do with it](01_command_line.md) pass  

   ### Scenario: [rules file found, but nothing to do with it](01_command_line.md): 
   - OK : When I run `./acc rules.txt`   
   - OK : Then I get   
   - [X] scenario   [rules file found, but nothing to do with it](01_command_line.md) pass  

   ### Scenario: [template creation when there's already one](01_command_line.md): 
   - OK : Given I run `./acc -ct` Successfully  
   - OK : When running once more `./acc --create_template`    
   - OK : Then I get   
   - [X] scenario   [template creation when there's already one](01_command_line.md) pass  

   ### Scenario: [-ar without rule](01_command_line.md): 
   - OK : When I run `./acc -ar`  or  `./acc --append_rule`    
   - OK : Then I get   
   - [X] scenario   [-ar without rule](01_command_line.md) pass  

   ### Scenario: [template creation (-ct and --create_template) 1/2](01_command_line.md): 
   - OK : Given there is no `template.ac` file  
   - OK : When I run `./acc -ct`  
   - OK : Then file `template.ac` is  
   - [X] scenario   [template creation (-ct and --create_template) 1/2](01_command_line.md) pass  

   ### Scenario: [template creation (-ct and --create_template) 2/2](01_command_line.md): 
   - OK : Given there is no `template.ac` file  
   - OK : When I run `./acc --create_template`  
   - OK : Then file `template.ac` is  
   - [X] scenario   [template creation (-ct and --create_template) 2/2](01_command_line.md) pass  


# Document: [02_source_list.md](02_source_list.md)  
  ## Feature: --list_files option and sources finding feature  
   ### Background: [](02_source_list.md): 
   - OK : Given there is no dir `dir1`   
   - OK : Given there is no dir `dir2`   
   - OK : Given there is no dir `dir3`   
   - OK : Given there is no dir `dira`   
   - OK : Given there is no dir `dirb`   
   - [X] background [](02_source_list.md) pass  

   ### Scenario: [Non recursive file identification test](02_source_list.md): 
   - OK : Given the new `./dir1/` dir  
   - OK : Given the new `./dir2/` dir  
   - OK : Given the new `./dir3/` dir  
   - OK : Given the file `./dir3/c-d.ads`  
   - OK : And   the file `./dir3/c.ads`      
   - OK : And   the file `./dir1/a.ads`      
   - OK : And   the file `./dir1/a.adb`      
   - OK : And   the file `./dir2/b.ads`      
   - OK : When I run `./acc -I dir1 -I dir2 -I dir3 --list_files`    
   - OK : Then the output is   
   - [X] scenario   [Non recursive file identification test](02_source_list.md) pass  

   ### Background: [](02_source_list.md): 
   - OK : Given there is no dir `dir1`   
   - OK : Given there is no dir `dir2`   
   - OK : Given there is no dir `dir3`   
   - OK : Given there is no dir `dira`   
   - OK : Given there is no dir `dirb`   
   - [X] background [](02_source_list.md) pass  

   ### Scenario: [Recursive file identification test](02_source_list.md): 
   - OK : Given the `./dira/` dir  
   - OK : Given the `./dirb/` dir  
   - OK : Given the `./dira/dira1/` dir  
   - OK : Given the `./dirb/dirb1/` dir  
   - OK : Given the file `./dira/a.ads`  
   - OK : And   the file `./dirb/b.ads`      
   - OK : And   the file `./dirb/dirb1/c.ads`      
   - OK : And   the file `./dira/dira1/c-d.ads`      
   - OK : When I run `./acc -I dira -Ir dirb --list_files`    
   - OK : Then the output is   
   - [X] scenario   [Recursive file identification test](02_source_list.md) pass  


# Document: [03_dependency_list.md](03_dependency_list.md)  
  ## Feature: Dependencies identification simple test suite  
   ### Scenario: [Simple test](03_dependency_list.md): 
   - OK : Given the `./dir1/` dir  
   - OK : Given the file `./dir1/a.ads`  
   - OK : And   the file `./dir1/a.adb`      
   - OK : And   the file `./dir1/b.adb`      
   - OK : And   the file `./dir1/c.ads`      
   - OK : And   the file `./dir1/c-d.adb`      
   - OK : When I run `./acc -I dir1 --list_dependencies`  
   - OK : Then the output is (unordered)   
   - [X] scenario   [Simple test](03_dependency_list.md) pass  

   ### Scenario: [Source with weird formatted withed unit](03_dependency_list.md): 
   - OK : Given the new file `./dir1/c-d.adb`      
   - OK : When I run `./acc -I dir1 --list_dependencies`  
   - OK : Then the output is (unordered)  
   - [X] scenario   [Source with weird formatted withed unit](03_dependency_list.md) pass  


# Document: [04_component_list.md](04_component_list.md)  
  ## Feature: Component definition rules test suite  
   ### Scenario: [One component list](04_component_list.md): 
   - OK : Given the file `rules.1`  
   - OK : when I run `./acc --list_rules rules.1`  
   - OK : Then I get   
   - [X] scenario   [One component list](04_component_list.md) pass  

   ### Scenario: [GUI component contains 3 other components, declared one by one on the rules file](04_component_list.md): 
   - OK : Given the file `rules.2`  
   - OK : when I run `./acc --list_rules rules.2`  
   - OK : Then I get   
   - [X] scenario   [GUI component contains 3 other components, declared one by one on the rules file](04_component_list.md) pass  

   ### Scenario: [GUI component contains 3 other components, declared all in one line in the rules file](04_component_list.md): 
   - OK : Given the file `rules.3`  
   - OK : when I run `./acc --list_rules rules.3`  
   - OK : Then I get   
   - [X] scenario   [GUI component contains 3 other components, declared all in one line in the rules file](04_component_list.md) pass  


# Document: [05_layer_rule.md](05_layer_rule.md)  
  ## Feature: Layer rules test suite  
   ### Scenario: [Sanity test, the Batik project architecture](05_layer_rule.md): 
   - OK : Given the file `rules.B`  
   - OK : When I run `./create_pkg Browser       spec -in dirB -with UI_Component`  
   - OK : When I run `./create_pkg Rasterizer    spec -in dirB -with Transcoder`  
   - OK : When I run `./create_pkg UI_Component  spec -in dirB -with Bridge -with Renderer`  
   - OK : When I run `./create_pkg Transcoder    spec -in dirB -with Bridge -with Renderer`  
   - OK : When I run `./create_pkg Bridge        spec -in dirB -with GVT -with SVGDOM`  
   - OK : When I run `./create_pkg Renderer      spec -in dirB -with GVT`  
   - OK : When I run `./create_pkg GVT           spec -in dirB`  
   - OK : When I run `./create_pkg SVGDOM        spec -in dirB -with SVG_Parser`  
   - OK : When I run `./create_pkg SVG_Parser    spec -in dirB`  
   - OK : When I run `./create_pkg SVG_Generator spec -in dirB -with SVGDOM`  
   - OK : When I run `./acc -q -I dirB rules.B`  
   - OK : Then I get no output  
   - OK : And  I get no error  
   - [X] scenario   [Sanity test, the Batik project architecture](05_layer_rule.md) pass  

   ### Scenario: [Base normal situation](05_layer_rule.md): 
   - OK : Given the file `rules.1`  
   - OK : Given the `./dir1/` dir  
   - OK : When I run `./create_pkg P1 spec  -in dir1 -with P2`  
   - OK : When I run `./create_pkg P2 spec  -in dir1 -with P3`  
   - OK : When I run `./create_pkg P2 body  -in dir1 -with P4`  
   - OK : When I run `./create_pkg P3 spec  -in dir1`  
   - OK : When I run `./create_pkg P4 spec  -in dir1`  
   - OK : When I run `./acc -q -I dir1 rules.1`  
   - OK : Then I get no output  
   - OK : And  I get no error  
   - [X] scenario   [Base normal situation](05_layer_rule.md) pass  

   ### Scenario: [Illegal upward dependency](05_layer_rule.md): 
   - OK : Given the file `rules.2`  
   - OK : When I run `./create_pkg P1 spec  -in dir2 -with P2`  
   - OK : When I run `./create_pkg P2 spec  -in dir2 -with P3 -with P4`  
   - OK : When I run `./create_pkg P3 spec  -in dir2`  
   - OK : When I run `./create_pkg P4 spec  -in dir2 -with P5`  
   - OK : When I run `./create_pkg P5 spec  -in dir2`  
   - OK : When I run `./acc --quiet -I dir2 rules.2`  
   - OK : Then I get  
   - [X] scenario   [Illegal upward dependency](05_layer_rule.md) pass  

   ### Scenario: [Layer bridging](05_layer_rule.md): 
   - OK : Given the file `rules.3`  
   - OK : When I run `./create_pkg P1 spec  -in dir3a -with P2`  
   - OK : When I run `./create_pkg P2 spec  -in dir3a`  
   - OK : When I run `./create_pkg P2 body  -in dir3a -with P3 -with P4`  
   - OK : When I run `./create_pkg P3 spec  -in dir3b`  
   - OK : When I run `./create_pkg P4 spec  -in dir3b`  
   - OK : When I run `./create_pkg P6 spec  -in dir3c`  
   - OK : When I run `./create_pkg P6 body  -in dir3c -with P4`  
   - OK : When I run `./acc -I dir3a -I dir3b -I dir3c rules.3`  
   - OK : Then I get  
   - [X] scenario   [Layer bridging](05_layer_rule.md) pass  

   ### Scenario: [Using a package that is neither in the same layer, nor in the visible layer](05_layer_rule.md): 
   - OK : Given the file `rules.4`  
   - OK : When I run `./create_pkg P1 spec  -in dir4 -with P2`  
   - OK : When I run `./create_pkg P2 spec  -in dir4 -with P3 -with P4 -with P7`  
   - OK : When I run `./create_pkg P3 spec  -in dir4`  
   - OK : When I run `./create_pkg P4 spec  -in dir4`  
   - OK : When I run `./create_pkg P7 spec  -in dir4`  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then I get  
   - [X] scenario   [Using a package that is neither in the same layer, nor in the visible layer](05_layer_rule.md) pass  


# Document: [06_child_packages.md](06_child_packages.md)  
  ## Feature: Child packages test suite  
   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Rules OK test, no output expected](06_child_packages.md): 
   - OK : When I run `./create_pkg GuI.P1 spec  -in dir1 -with GUI.P2`  
   - OK : When I run `./create_pkg GUI.P2 spec  -in dir1 -with DB.p3 -with DB.P4`  
   - OK : When I run `./create_pkg Db.P3  spec  -in dir1`  
   - OK : When I run `./create_pkg DB.P4  spec  -in dir1`  
   - OK : When I run `./acc -q -I dir1 rules.txt`  
   - OK : Then I get no error  
   - OK : And  I get no output  
   - [X] scenario   [Rules OK test, no output expected](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Reverse dependency test](06_child_packages.md): 
   - OK : When I run `./create_pkg GUI.P1 spec  -in dir2 -with GUI.P2`  
   - OK : When I run `./create_pkg GUI.P2 spec  -in dir2 -with DB.P3 -with DB.P4`  
   - OK : When I run `./create_pkg DB.P3  spec  -in dir2`  
   - OK : When I run `./create_pkg DB.P4  spec  -in dir2`  
   - OK : When I run `./create_pkg DB.P4  body  -in dir2 -with GUI.P5`  
   - OK : When I run `./create_pkg GUI.P5 spec  -in dir2`  
   - OK : When I run `./acc -q -I dir2 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Reverse dependency test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Layer bridging test](06_child_packages.md): 
   - OK : When I run `./create_pkg GUI.P1 spec  -in dir3 -with GUI.P2`  
   - OK : When I run `./create_pkg GUI.P2 spec  -in dir3 -with DB.P3 -with DB.P4`  
   - OK : When I run `./create_pkg DB.P3  spec  -in dir3`  
   - OK : When I run `./create_pkg DB.P4  spec  -in dir3`  
   - OK : When I run `./create_pkg P6     spec  -in dir3 -with DB.P4`  
   - OK : When I run `./acc -I dir3 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Layer bridging test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Undescribed dependency test](06_child_packages.md): 
   - OK : When I run `./create_pkg GUI.P1 spec  -in dir4 -with GUI.P2`  
   - OK : When I run `./create_pkg GUI.P2 spec  -in dir4 -with DB.P3 -with DB.P4 -with P7`  
   - OK : When I run `./create_pkg DB.P3  spec  -in dir4`  
   - OK : When I run `./create_pkg DB.P4  spec  -in dir4`  
   - OK : When I run `./create_pkg P7     spec  -in dir4`  
   - OK : When I run `./acc -I dir4 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Undescribed dependency test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Packages in the same layer may with them self](06_child_packages.md): 
   - OK : When I run `./create_pkg GUI.P1 spec  -in dir5`  
   - OK : When I run `./create_pkg GUI.P2 spec  -in dir5 -with GUI.P1`  
   - OK : When I run `./create_pkg DB.P3  spec  -in dir5`  
   - OK : When I run `./create_pkg DB.P4  spec  -in dir5 -with DB.P3`  
   - OK : When I run `./acc -I dir5 rules.txt`  
   - OK : Then I get no error  
   - OK : And  I get no output  
   - [X] scenario   [Packages in the same layer may with them self](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg](06_child_packages.md): 
   - OK : When I run `./create_pkg GUI.P1 spec  -in dir6 -with DB.P1`  
   - OK : When I run `./create_pkg GUIP2  spec  -in dir6 -with DB.P1`  
   - OK : When I run `./create_pkg DB.P1  spec  -in dir6`  
   - OK : When I run `./create_pkg DBP2   spec  -in dir6 -with DB.P1`  
   - OK : When I run `./acc -I dir6 rules.txt`  
   - OK : Then I get (unordered)  
   - [X] scenario   [GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Forbidding Interfaces but allowing Interfaces.C](06_child_packages.md): 
   - OK : Given the file `rules7.txt`  
   - OK : When I run `./create_pkg Interfaces   spec -in dir7`  
   - OK : When I run `./create_pkg Interfaces.C spec -in dir7`  
   - OK : When I run `./create_pkg P1           spec -in dir7 -with Interfaces`  
   - OK : When I run `./create_pkg P2           spec -in dir7 -with Interfaces.C`  
   - OK : When I run `./create_pkg P3           spec -in dir7 -with Interfaces.Java`  
   - OK : When I run `./acc -I dir7 rules7.txt`  
   - OK : Then I get (unordered)  
   - [X] scenario   [Forbidding Interfaces but allowing Interfaces.C](06_child_packages.md) pass  


# Document: [07_rules_files_syntax.md](07_rules_files_syntax.md)  
  ## Feature: Rules file syntax test suite  
   ### Scenario: [Reference file](07_rules_files_syntax.md): 
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc --list_rules rules1.txt`    
   - OK : then I get  
   - [X] scenario   [Reference file](07_rules_files_syntax.md) pass  

   ### Scenario: [Casing](07_rules_files_syntax.md): 
   - OK : Given the file `rules2.txt`  
   - OK : When I run `./acc -lr rules2.txt`    
   - OK : then I get  
   - [X] scenario   [Casing](07_rules_files_syntax.md) pass  

   ### Scenario: [Spacing and comments](07_rules_files_syntax.md): 
   - OK : Given the file `rules3.txt`  
   - OK : When I run `./acc -lr rules3.txt`    
   - OK : then I get  
   - [X] scenario   [Spacing and comments](07_rules_files_syntax.md) pass  


# Document: [08_globbing_characters.md](08_globbing_characters.md)  
  ## Feature: Globbing Character test suite  
   ### Scenario: [rules test](08_globbing_characters.md): 
   - OK : Given the file `rules.1`  
   - OK : When I run `./create_pkg P1 spec -in dir1 -with P2`  
   - OK : When I run `./create_pkg P2 spec  -in dir1 -with P3 -with P4`   
   - OK : When I run `./create_pkg P3 spec  -in dir1 -with Interfaces.C`  
   - OK : When I run `./create_pkg P4 spec  -in dir1 -with Interfaces.C.Strings`  
   - OK : When I run `./acc -I dir1 rules.1`   
   - OK : Then there is no error  
   - OK : And  there is no output  
   - [X] scenario   [rules test](08_globbing_characters.md) pass  

   ### Scenario: [illegal use of Interfaces from Application_Layer](08_globbing_characters.md): 
   - OK : Given the file `rules.2`  
   - OK : When I run `./create_pkg P1 spec -in dir2 -with P2 -with Interfaces.C`  
   - OK : When I run `./create_pkg P2 spec -in dir2 -with P3 -with P4`   
   - OK : When I run `./create_pkg P3 spec -in dir2 -with Interfaces.C`  
   - OK : When I run `./create_pkg P4 spec -in dir2 -with Interfaces.C.Strings`  
   - OK : When I run `./acc -I dir2 rules.2`   
   - OK : Then I get  
   - [X] scenario   [illegal use of Interfaces from Application_Layer](08_globbing_characters.md) pass  


# Document: [09_gtkada.md](09_gtkada.md)  
  ## Feature: GtkAda test suite  
   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : Given I run `unzip -q 09_GtkAda/gtkada-master.zip` Successfully  
   - OK : Given I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [File Identification](09_gtkada.md): 
   - OK : Given I run `find gtkada-master -name "*.ad[sb]"` Successfully  
   - OK : When I run `./acc -q -lf -r -I gtkada-master` Successfully  
   - OK : Then I get file (unordered) `09_GtkAda/expected_output.1`  
   - [X] scenario   [File Identification](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : Given I run `unzip -q 09_GtkAda/gtkada-master.zip` Successfully  
   - OK : Given I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [Unit Identification](09_gtkada.md): 
   - OK : When I run `./acc -ld -r -I gtkada-master` Successfully  
   - OK : Then I get file (unordered) `09_GtkAda/expected_output.2`  
   - [X] scenario   [Unit Identification](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : Given I run `unzip -q 09_GtkAda/gtkada-master.zip` Successfully  
   - OK : Given I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [A realistic GtkAda description file](09_gtkada.md): 
   - OK : Given there is a file `09_GtkAda/GtkAda.ac`  
   - OK : When I run `./acc 09_GtkAda/GtkAda.ac -r -I gtkada-master` Successfully  
   - OK : Then I get file (unordered) `09_GtkAda/expected_output.3`  
   - [X] scenario   [A realistic GtkAda description file](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : Given I run `unzip -q 09_GtkAda/gtkada-master.zip` Successfully  
   - OK : Given I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [Another realistic GtkAda description file](09_gtkada.md): 
   - OK : Given there is a file `09_GtkAda/GtkAda2.ac`  
   - OK : When I run `./acc 09_GtkAda/GtkAda2.ac -q -r -I gtkada-master` Successfully  
   - OK : Then I get file (unordered) `09_GtkAda/expected_output.4`  
   - [X] scenario   [Another realistic GtkAda description file](09_gtkada.md) pass  


# Document: [10_use_rule.md](10_use_rule.md)  
  ## Feature: Use rules test suite  
   ### Scenario: [May_Use rule, code compliant, no output expected](10_use_rule.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir1 -with P2` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir1 -with P3` Successfully  
   - OK : Given I run `./create_pkg P2 body -in dir1 -with P4` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir1` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir1` Successfully  
   - OK : Given the file `rules.1`  
   - OK : When I run `./acc -q -I dir1 rules.1`  
   - OK : Then I get no output  
   - [X] scenario   [May_Use rule, code compliant, no output expected](10_use_rule.md) pass  

   ### Scenario: [Using a unit from a non allowed unit](10_use_rule.md): 
   - OK : Given there is no `dir2` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir2 -with P2` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir2 -with P3` Successfully  
   - OK : Given I run `./create_pkg P2 body -in dir2 -with P4` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P1 body -in dir2 -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg P4 body -in dir2 -with Interfaces.C` Successfully  
   - OK : Given the new file `rules.1`  
   - OK : When I run `./acc -q -I dir2 rules.1`  
   - OK : Then output is  
   - [X] scenario   [Using a unit from a non allowed unit](10_use_rule.md) pass  

   ### Scenario: [Forbidden use test](10_use_rule.md): 
   - OK : Given there is no `dir3` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir3 -with P2` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir3 -with P3` Successfully  
   - OK : Given I run `./create_pkg P2 body -in dir3 -with P4` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir3` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir3` Successfully  
   - OK : Given the file `rules.3`  
   - OK : When I run `./acc -q -I dir3 rules.3`  
   - OK : Then output is  
   - [X] scenario   [Forbidden use test](10_use_rule.md) pass  

   ### Scenario: [Allowing use of an environnement package](10_use_rule.md): 
   - OK : Given there is no `dir4` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir4 -with P2` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir4 -with Containers.Generic_Sort` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir4` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir4` Successfully  
   - OK : Given the file `rules.4`  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then output is  
   - OK : Given the new file `rules.4`  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then I get no output  
   - [X] scenario   [Allowing use of an environnement package](10_use_rule.md) pass  

   ### Scenario: [Cumulative only ... may use X rules](10_use_rule.md): 
   - OK : Given there is no `dir5` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : Given the file `rules.5`  
   - OK : When I run `./acc -I dir5 rules.5`  
   - OK : Then output is  
   - [X] scenario   [Cumulative only ... may use X rules](10_use_rule.md) pass  

   ### Scenario: [Combining Allowed and Forbidden](10_use_rule.md): 
   - OK : Given there is no `dir6` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir6 -with Interfaces` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir6 -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir6 -with Interfaces.Java` Successfully  
   - OK : Given the file `rules.6`  
   - OK : When I run `./acc -I dir6 rules.6`  
   - OK : Then output is (unordered)  
   - OK : Given the new file `rules.6b`  
   - OK : When I run `./acc -I dir6 rules.6b`  
   - OK : Then I get no output  
   - [X] scenario   [Combining Allowed and Forbidden](10_use_rule.md) pass  

   ### Scenario: [X may use Unit List rules](10_use_rule.md): 
   - OK : Given there is no `dir7` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir7 -with P2 -with P3 -with P4` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir7 -with P1` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir7 -with P1` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir7 -with P1` Successfully  
   - OK : Given the file `rules.7`  
   - OK : When I run `./acc -I dir7 rules.7`  
   - OK : Then output is (unordered)  
   - [X] scenario   [X may use Unit List rules](10_use_rule.md) pass  

   ### Scenario: [only X may use Unit List rules](10_use_rule.md): 
   - OK : Given there is no `dir8` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir8 -with P2 -with P3` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir8 -with P2 -with P3` Successfully  
   - OK : Given the file `rules.8`  
   - OK : When I run `./acc -I dir8 rules.8`  
   - OK : Then output is  
   - [X] scenario   [only X may use Unit List rules](10_use_rule.md) pass  

   ### Scenario: [Appending rules](10_use_rule.md): 
   - OK : Given there is no `dir9` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir9` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir9 -with P2` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : Given I run `./create_pkg Bus spec -in dir9` Successfully  
   - OK : Given I run `./create_pkg IO spec -in dir9` Successfully  
   - OK : Given the file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 -ar "only P1 may use IO" rules.9`  
   - OK : Then output is  
   - OK : Given the new file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9`  
   - OK : Then output is  
   - [X] scenario   [Appending rules](10_use_rule.md) pass  


# Document: [11_batik.md](11_batik.md)  
  ## Feature: Batik test suite  
   ### Scenario: [--list_file test](11_batik.md): 
   - OK : Given there is no `batik-1.9` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : When I run `./acc -lf -Ir ./batik-1.9`  
   - OK : Then I get file (unordered) `11_Batik/expected_output.1`  
   - [X] scenario   [--list_file test](11_batik.md) pass  

   ### Scenario: [public class](11_batik.md): 
   - OK : Given there is no `dir2` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : Given I run `mkdir -p dir2` Successfully  
   - OK : Given I run `cp ./batik-1.9/contrib/jsvg/JSVG.java dir2` Successfully  
   - OK : When I run `./acc -ld -I dir2`  
   - OK : Then I get file `11_Batik/expected_output.2`  
   - [X] scenario   [public class](11_batik.md) pass  

   ### Scenario: [public interface class](11_batik.md): 
   - OK : Given there is no `dir3` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : Given I run `mkdir -p dir3` Successfully  
   - OK : Given I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3` Successfully  
   - OK : When I run `./acc -ld -I dir3`  
   - OK : Then I get file `11_Batik/expected_output.3`  
   - [X] scenario   [public interface class](11_batik.md) pass  

   ### Scenario: [no import](11_batik.md): 
   - OK : Given there is no `dir4` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : Given I run `mkdir -p dir4` Successfully  
   - OK : Given I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4` Successfully  
   - OK : When I run `./acc -ld -I dir4`  
   - OK : Then I get file `11_Batik/expected_output.4`  
   - [X] scenario   [no import](11_batik.md) pass  

   ### Scenario: [no package](11_batik.md): 
   - OK : Given there is no `dir5` directory  
   - OK : Given I run `mkdir -p dir5` Successfully  
   - OK : Given the file `dir5/MyClass.java`  
   - OK : When I run `./acc -ld -I dir5`  
   - OK : Then I get file `11_Batik/expected_output.5`  
   - [X] scenario   [no package](11_batik.md) pass  

   ### Scenario: [public abstract class](11_batik.md): 
   - OK : Given there is no `dir6` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : Given I run `mkdir -p dir6` Successfully  
   - OK : Given I run `cp ./batik-1.9/batik-transcoder/src/main/java/org/apache/batik/transcoder/SVGAbstractTranscoder.java dir6` Successfully  
   - OK : Given there is a file `11_Batik/rules.B`  
   - OK : When I run `./acc 11_Batik/rules.B -q -I dir6`  
   - OK : Then I get file `11_Batik/expected_output.6`  
   - [X] scenario   [public abstract class](11_batik.md) pass  

   ### Scenario: [Let's add dependencies to Browser and Rasterizer into a Transcoder class](11_batik.md): 
   - OK : Given there is no `dir7` directory  
   - OK : Given I run `mkdir -p dir7` Successfully  
   - OK : Given the file `dir7/MyClass.java`  
   - OK : Given the file `rules.7`  
   - OK : When I run `./acc rules.7 -I dir7`  
   - OK : Then I get file `11_Batik/expected_output.7`  
   - [X] scenario   [Let's add dependencies to Browser and Rasterizer into a Transcoder class](11_batik.md) pass  

   ### Scenario: [-ld test](11_batik.md): 
   - OK : Given there is no `batik-1.9` directory  
   - OK : Given I run `tar -xf 11_Batik/batik-src-1.9.tar.gz` Successfully  
   - OK : When I run `./acc -ld -Ir ./batik-1.9`  
   - OK : Then I get file (unordered) `11_Batik/expected_output.8`  
   - [X] scenario   [-ld test](11_batik.md) pass  


# Document: [12_zipada.md](12_zipada.md)  
  ## Feature: ZipAda code test suite  
   ### Scenario: [-lf test](12_zipada.md): 
   - OK : Given there is no `zip-ada` directory  
   - OK : Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully  
   - OK : Given there is a file `12_ZipAda/expected_output.1`  
   - OK : Given there is a file `12_ZipAda/zipadarules.txt`  
   - OK : When I run `./acc -lf -r -I zip-ada` Successfully  
   - OK : Then I get file `12_ZipAda/expected_output.1`  
   - [X] scenario   [-lf test](12_zipada.md) pass  

   ### Scenario: [-ld test](12_zipada.md): 
   - OK : Given there is no `zip-ada` directory  
   - OK : Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully  
   - OK : Given there is a file `12_ZipAda/expected_output.2`  
   - OK : When I run `./acc -ld -r -I ./zip-ada` Successfully  
   - OK : Then I get file (unordered) `12_ZipAda/expected_output.2`  
   - [X] scenario   [-ld test](12_zipada.md) pass  

   ### Scenario: [rules test](12_zipada.md): 
   - OK : Given there is no `zip-ada` directory  
   - OK : Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully  
   - OK : Given there is a file `12_ZipAda/zipadarules.txt`  
   - OK : When I run `./acc 12_ZipAda/zipadarules.txt -r -I ./zip-ada` Successfully  
   - OK : Then I get no output  
   - [X] scenario   [rules test](12_zipada.md) pass  


# Document: [13_ada_units.md](13_ada_units.md)  
  ## Feature: Ada units test suite  
   ### Scenario: [Ada compilation units unit test](13_ada_units.md): 
   - OK : Given I run `gnat make -q sub-test -I13_Ada_Units/src -D13_Ada_Units/src` Successfully  
   - OK : Given there is a file `13_Ada_Units/expected_output.1`  
   - OK : When I run `./acc -ld -I 13_Ada_Units/src` Successfully  
   - OK : Then I get file (unordered) `13_Ada_Units/expected_output.1`  
   - [X] scenario   [Ada compilation units unit test](13_ada_units.md) pass  


# Document: [14_rules_on_components.md](14_rules_on_components.md)  
  ## Feature: Component processing in rules unit test  
   ### Scenario: [Layer component](14_rules_on_components.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully  
   - OK : Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg LC spec -in src` Successfully  
   - OK : Given the file `rules1a.txt`  
   - OK : When I run `./acc rules1a.txt -I src`  
   - OK : Then output is  
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully  
   - OK : Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg LC spec -in src` Successfully  
   - OK : Given the file `rules1b.txt`  
   - OK : When I run `./acc rules1b.txt -I src`  
   - OK : Then I get no output  
   - [X] scenario   [Layer component](14_rules_on_components.md) pass  

   ### Scenario: [Env component allowed](14_rules_on_components.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully  
   - OK : Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully  
   - OK : Given I run `./create_pkg LC spec -in src` Successfully  
   - OK : Given the file `rules2.txt`  
   - OK : When I run `./acc rules2.txt -I src`  
   - OK : Then I get no output  
   - [X] scenario   [Env component allowed](14_rules_on_components.md) pass  

   ### Scenario: [Trying to include a unit in more components](14_rules_on_components.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg P1 spec -in src` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in src` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in src` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in src` Successfully  
   - OK : Given the file `rules3.txt`  
   - OK : When I run `./acc rules3.txt -I src`  
   - OK : Then output is  
   - [X] scenario   [Trying to include a unit in more components](14_rules_on_components.md) pass  

   ### Scenario: [Test on Components embedding components embedding components...](14_rules_on_components.md): 
   - OK : Given there is no `dir4` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir4 -with Ada.Containers -with Interfaces.C` Successfully  
   - OK : Given the file `rules4.txt`  
   - OK : When I run `./acc rules4.txt -I dir4`  
   - OK : Then output is  
   - OK : Given the new file `rules4.txt`  
   - OK : When I run `./acc rules4.txt -I dir4`  
   - OK : Then I get no output  
   - [X] scenario   [Test on Components embedding components embedding components...](14_rules_on_components.md) pass  

   ### Scenario: [Test A B C example posted on fr.comp.lang.ada...](14_rules_on_components.md): 
   - OK : Given there is no `dir5` directory  
   - OK : Given I run `./create_pkg X.P1 spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg Y.P1 spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg Y.P2 spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg Z.P1 spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg Z.P2 spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg U spec -in dir5` Successfully  
   - OK : Given I run `./create_pkg V spec -in dir5` Successfully  
   - OK : Given the new file `dir5/y-p1.ads`  
   - OK : Given the new file `dir5/z-p1.ads`  
   - OK : Given the new file `dir5/z-p2.ads`  
   - OK : Given the new file `dir5/u.ads`  
   - OK : Given the new file `dir5/v.ads`  
   - OK : Given the file `rules5.txt`  
   - OK : When I run `./acc rules5.txt -I dir5`  
   - OK : Then output is  
   - [X] scenario   [Test A B C example posted on fr.comp.lang.ada...](14_rules_on_components.md) pass  


# Document: [15_precedences_rules.md](15_precedences_rules.md)  
  ## Feature: Precedence rules test suite  
   ### Scenario: [Declaration of a component already existing in code](15_precedences_rules.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg LC.Z spec -in src` Successfully  
   - OK : Given the new file `src/la-x.ads`  
   - OK : Given the new file `src/lb-y.ads`  
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc rules1.txt -I src`  
   - OK : Then output is  
   - [X] scenario   [Declaration of a component already existing in code](15_precedences_rules.md) pass  

   ### Scenario: [Alowing a child of forbidden unit](15_precedences_rules.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `./create_pkg LC.Z spec -in src` Successfully  
   - OK : Given the new file `src/la-x.ads`  
   - OK : Given the new file `src/lb-y.ads`  
   - OK : Given the file `rules2.txt`  
   - OK : When I run `./acc rules2.txt -I src`  
   - OK : Then output is  
   - [X] scenario   [Alowing a child of forbidden unit](15_precedences_rules.md) pass  


# Document: [16_adactl.md](16_adactl.md)  
  ## Feature: AdaControl code test suite  
   ### Scenario: [-lf test](16_adactl.md): 
   - OK : Given there is no `adactl-1.19r10` directory  
   - OK : Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully  
   - OK : Given there is a file `16_AdaControl/expected_output.1`  
   - OK : When I run `./acc -lf -r -I adactl-1.19r10/src` Successfully  
   - OK : Then I get file (unordered) `16_AdaControl/expected_output.1`  
   - [X] scenario   [-lf test](16_adactl.md) pass  

   ### Scenario: [-ld test](16_adactl.md): 
   - OK : Given there is no `adactl-1.19r10` directory  
   - OK : Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully  
   - OK : Given there is a file `16_AdaControl/expected_output.2`  
   - OK : When I run `./acc -ld -r -I ./adactl-1.19r10/src` Successfully  
   - OK : Then I get file (unordered) `16_AdaControl/expected_output.2`  
   - [X] scenario   [-ld test](16_adactl.md) pass  

   ### Scenario: [rules test](16_adactl.md): 
   - OK : Given there is no `adactl-1.19r10` directory  
   - OK : Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully  
   - OK : Given there is a file `16_AdaControl/adactl.ac`  
   - OK : When I run `./acc 16_AdaControl/adactl.ac -r -I ./adactl-1.19r10/src` Successfully  
   - OK : Then I get file `16_AdaControl/expected_output.3`  
   - [X] scenario   [rules test](16_adactl.md) pass  


# Document: [17_acc.md](17_acc.md)  
  ## Feature: Acc code test suite  
   ### Scenario: [-lf test](17_acc.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `unzip -q -o 17_Acc/src.zip -d src` Successfully  
   - OK : Given there is a file `17_Acc/expected_output.1`  
   - OK : When I run `./acc -lf -I src` Successfully  
   - OK : Then I get file (unordered) `17_Acc/expected_output.1`  
   - [X] scenario   [-lf test](17_acc.md) pass  

   ### Scenario: [-ld test](17_acc.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `unzip -q -o 17_Acc/src.zip -d src` Successfully  
   - OK : Given there is a file `17_Acc/expected_output.2`  
   - OK : When I run `./acc -ld -I ./src` Successfully  
   - OK : Then I get file (unordered) `17_Acc/expected_output.2`  
   - [X] scenario   [-ld test](17_acc.md) pass  

   ### Scenario: [rules test](17_acc.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `unzip -q -o 17_Acc/src.zip -d src` Successfully  
   - OK : Given there is a file `17_Acc/archicheck.ac`  
   - OK : Given there is a file `17_Acc/expected_output.3`  
   - OK : When I run `./acc 17_Acc/archicheck.ac -I ./src` Successfully  
   - OK : Then I get file `17_Acc/expected_output.3`  
   - [X] scenario   [rules test](17_acc.md) pass  

   ### Scenario: [--list_non_covered](17_acc.md): 
   - OK : Given there is no `src` directory  
   - OK : Given I run `unzip -q -o 17_Acc/src.zip -d src` Successfully  
   - OK : Given there is a file `17_Acc/archicheck.ac`  
   - OK : Given there is a file `17_Acc/expected_output.4`  
   - OK : When I run `./acc 17_Acc/archicheck.ac -lnc -I ./src` Successfully  
   - OK : Then I get file `17_Acc/expected_output.4`  
   - [X] scenario   [--list_non_covered](17_acc.md) pass  


# Document: [18_petclinic.md](18_petclinic.md)  
  ## Feature: Spring Pet Clinic code test suite  
   ### Scenario: [-lf test](18_petclinic.md): 
   - OK : Given there is no `src1` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-petclinic-master src1` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/expected_output.1`  
   - OK : When I run `./acc -lf -r -I src1` Successfully  
   - OK : Then I get file `18_Spring_PetClinic/expected_output.1`  
   - [X] scenario   [-lf test](18_petclinic.md) pass  

   ### Scenario: [-ld test](18_petclinic.md): 
   - OK : Given there is no `src1` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-petclinic-master src1` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/expected_output.2`  
   - OK : When I run `./acc -ld -r -I ./src1` Successfully  
   - OK : Then I get file (unordered) `18_Spring_PetClinic/expected_output.2`  
   - [X] scenario   [-ld test](18_petclinic.md) pass  

   ### Scenario: [rules test](18_petclinic.md): 
   - OK : Given there is no `src1` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-petclinic-master src1` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/petclinic.ac`  
   - OK : When I run `./acc 18_Spring_PetClinic/petclinic.ac -r -I ./src1` Successfully  
   - OK : Then I get file `18_Spring_PetClinic/expected_output.3`  
   - [X] scenario   [rules test](18_petclinic.md) pass  

   ### Scenario: [--list_non_covered](18_petclinic.md): 
   - OK : Given there is no `src1` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-petclinic-master src1` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/petclinic.ac`  
   - OK : When I run `./acc 18_Spring_PetClinic/petclinic.ac -lnc -r -I ./src1` Successfully  
   - OK : Then I get file `18_Spring_PetClinic/expected_output.4`  
   - [X] scenario   [--list_non_covered](18_petclinic.md) pass  

   ### Scenario: [alternative rules test](18_petclinic.md): 
   - OK : Given there is no `src1` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-petclinic-master src1` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/alternative.ac`  
   - OK : When I run `./acc 18_Spring_PetClinic/alternative.ac -r -I ./src1` Successfully  
   - OK : Then I get file `18_Spring_PetClinic/expected_output.5`  
   - [X] scenario   [alternative rules test](18_petclinic.md) pass  

   ### Scenario: [Layered version of petclinic test](18_petclinic.md): 
   - OK : Given there is no `src2` directory  
   - OK : Given I run `unzip -q -o 18_Spring_PetClinic/spring-framework-petclinic-master.zip` Successfully  
   - OK : Given I run `mv spring-framework-petclinic-master src2` Successfully  
   - OK : Given there is a file `18_Spring_PetClinic/framework-petclinic.ac`  
   - OK : When I run `./acc 18_Spring_PetClinic/framework-petclinic.ac -r -I ./src2` Successfully  
   - OK : Then I get file `18_Spring_PetClinic/expected_output.6`  
   - [X] scenario   [Layered version of petclinic test](18_petclinic.md) pass  


# Document: [19_rules_src_coverage.md](19_rules_src_coverage.md)  
  ## Feature: Rules vs sources coverage test suite  
   ### Scenario: [Warnings on units appearing in rules file and not related to any source](19_rules_src_coverage.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir1` Successfully  
   - OK : Given I run `./create_pkg P2 spec -in dir1` Successfully  
   - OK : Given I run `./create_pkg P5 spec -in dir1` Successfully  
   - OK : Given the file `test1.ac`  
   - OK : When I run `./acc test1.ac -I ./dir1`  
   - OK : Then output is  
   - [X] scenario   [Warnings on units appearing in rules file and not related to any source](19_rules_src_coverage.md) pass  

   ### Scenario: [Non covered sources](19_rules_src_coverage.md): 
   - OK : Given there is no `dir2` directory  
   - OK : Given I run `./create_pkg P2 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P3 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P4 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P5 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg P1.X spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg Y.P1 spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg Framework.Utilities spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg Framework_Utilities spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg Java.Awt spec -in dir2` Successfully  
   - OK : Given I run `./create_pkg Java spec -in dir2` Successfully  
   - OK : Given the file `test2.ac`  
   - OK : When I run `./acc -lnc test2.ac -I ./dir2`  
   - OK : Then output is (unordered)  
   - [X] scenario   [Non covered sources](19_rules_src_coverage.md) pass  

   ### Scenario: [Case insensitivity of Is_A_Component function (non reg)](19_rules_src_coverage.md): 
   - OK : Given there is no `dir3` directory  
   - OK : Given I run `./create_pkg P1 spec -in dir3` Successfully  
   - OK : Given the file `test3.ac`  
   - OK : When I run `./acc test3.ac -I dir3`  
   - OK : Then output is  
   - [X] scenario   [Case insensitivity of Is_A_Component function (non reg)](19_rules_src_coverage.md) pass  


# Document: [20_C_sanity_check.md](20_C_sanity_check.md)  
  ## Feature: C sanity test suite  
   ### Background: [](20_C_sanity_check.md): 
   - OK : Given there is no `src` directory  
   - OK : Given there is no `include` directory  
   - [X] background [](20_C_sanity_check.md) pass  

   ### Scenario: [.c and .h files list](20_C_sanity_check.md): 
   - OK : Given the directory `include`  
   - OK : Given the file `include/newton_method.h`  
   - OK : Given the file `include/square_root.h`  
   - OK : Given the directory `src`  
   - OK : Given the file `src/main.c`  
   - OK : Given the file `src/newton_method.c`  
   - OK : Given the file `src/square_root.c`  
   - OK : When I run `./acc -lf -I include -I src`  
   - OK : Then output is  
   - [X] scenario   [.c and .h files list](20_C_sanity_check.md) pass  

   ### Background: [](20_C_sanity_check.md): 
   - OK : Given there is no `src` directory  
   - OK : Given there is no `include` directory  
   - [X] background [](20_C_sanity_check.md) pass  

   ### Scenario: [dependencies list](20_C_sanity_check.md): 
   - OK : Given the directory `include`  
   - OK : Given the file `include/newton_method.h`  
   - OK : Given the file `include/square_root.h`  
   - OK : Given the directory `src`  
   - OK : Given the file `src/main.c`  
   - OK : Given the file `src/newton_method.c`  
   - OK : Given the file `src/square_root.c`  
   - OK : When I run `./acc -ld -I include -I src`  
   - OK : Then output is (unordered)  
   - [X] scenario   [dependencies list](20_C_sanity_check.md) pass  


# Document: [21_independent_components.md](21_independent_components.md)  
  ## Feature: Independent Component test suite  
   ### Scenario: [Test Independent Components](21_independent_components.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given I run `./create_pkg X.P1 spec -in dir1` Successfully  
   - OK : Given I run `./create_pkg Bus spec -in dir1` Successfully  
   - OK : Given the new file `dir1/x-p2.ads`  
   - OK : Given the new file `dir1/y.ads`  
   - OK : Given the new file `dir1/u.ads`  
   - OK : Given the new file `dir1/v.ads`  
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc rules1.txt -I dir1`  
   - OK : Then I get no output  
   - [X] scenario   [Test Independent Components](21_independent_components.md) pass  

   ### Scenario: [Test broken Independent Components rule](21_independent_components.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given I run `./create_pkg X.P1 spec -in dir1` Successfully  
   - OK : Given I run `./create_pkg Bus spec -in dir1` Successfully  
   - OK : Given the new file `dir1/x-p2.ads`  
   - OK : Given the new file `dir1/y.ads`  
   - OK : Given the file `dir1/y-p3.ads`  
   - OK : Given the new file `dir1/u.ads`  
   - OK : Given the new file `dir1/v.ads`  
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc rules1.txt -I dir1`  
   - OK : Then output is  
   - [X] scenario   [Test broken Independent Components rule](21_independent_components.md) pass  


# Document: [22_files_component.md](22_files_component.md)  
  ## Feature: Adding compilation units through files to a Component  
   ### Scenario: [Sources identification through rules file (no -I on command line)](22_files_component.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given there is no `dir2` directory  
   - OK : Given the new directory `dir1`  
   - OK : Given I run `./create_pkg B body -in dir2` Successfully  
   - OK : Given the file `dir1/a.ads`  
   - OK : Given the file `dir1/a.adb`  
   - OK : Given the file `dir2/c.ads`  
   - OK : When I run `./acc -I dir1 --list_dependencies`  
   - OK : Then output is (unordered)  
   - [X] scenario   [Sources identification through rules file (no -I on command line)](22_files_component.md) pass  

   ### Scenario: [Source with weird formatted withed unit](22_files_component.md): 
   - OK : Given there is no `dir2` directory  
   - OK : Given the new directory `dir1`  
   - OK : Given I run `./create_pkg B body -in dir2` Successfully  
   - OK : Given the file `dir2/a.ads`  
   - OK : Given the file `dir2/a.adb`  
   - OK : Given the file `dir2/b.adb`  
   - OK : Given the file `dir2/c.ads`  
   - OK : Given the file `dir2/c-d.adb`  
   - OK : When I run `./acc -I dir2 --list_dependencies`  
   - OK : Then output is (unordered)  
   - [X] scenario   [Source with weird formatted withed unit](22_files_component.md) pass  


## Summary : **Success**, 94 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 94    |
| Empty      | 0     |
| Not Run    | 1     |

