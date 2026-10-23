
# Document: [01_command_line.md](01_command_line.md)  
   ### Scenario: [Help options](01_command_line.md): 
   - OK : When I successfully run `./acc -h`,   
   - OK : Then output is  
   - [X] scenario   [Help options](01_command_line.md) pass  

   ### Scenario: [Version option](01_command_line.md): 
   - OK : When I run `./acc --version`  
   - OK : Then the output matches `0\.[0-9]+\.[0-9]+.*`  
   - [X] scenario   [Version option](01_command_line.md) pass  

   ### Scenario: [-I option without src dir](01_command_line.md): 
   - OK : When running `./acc -I`,  
   - OK : Then I get  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [-I option without src dir](01_command_line.md) pass  

   ### Scenario: [-I option with an unknown dir](01_command_line.md): 
   - OK : When running `./acc -I qsdqjh`    
   - OK : Then I get  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [-I option with an unknown dir](01_command_line.md) pass  

   ### Scenario: [unknown -xyz option](01_command_line.md): 
   - OK : When running `./acc -xzy`    
   - OK : Then I get   
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [unknown -xyz option](01_command_line.md) pass  

   ### Scenario: [-I option with... nothing to do](01_command_line.md): 
   - OK : Given the directory `dir6`      
   - OK : And   the file `dir6/src.adb`  
   - OK : When I run `./acc -I dir6`      
   - OK : Then the output contains  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [-I option with... nothing to do](01_command_line.md) pass  

   ### Scenario: [-lr option without rules file](01_command_line.md): 
   - OK : When I run `./acc -lr  `  
   - OK : Then the output contains  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [-lr option without rules file](01_command_line.md) pass  

   ### Scenario: [Legal line, but no src file in the given (existing) directory](01_command_line.md): 
   - OK : Given the new directory `dir9`  
   - OK : When I run `./acc -lf -I dir9`    
   - OK : Then I get `Warning : Cannot list files, no sources found to analyze`  
   - OK : And  the exit code is `0`  
   - OK : And there is no error output  
   - [X] scenario   [Legal line, but no src file in the given (existing) directory](01_command_line.md) pass  

   ### Scenario: [file given to -I, instead of a directory](01_command_line.md): 
   - OK : Given new file `rules.txt` containing `Interfaces use is forbidden`  
   - OK : And   file `src.adb`  
   - OK : When I run `./acc rules.txt -I src.adb`  
   - OK : Then I get `Error : src.adb is not a directory`    
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [file given to -I, instead of a directory](01_command_line.md) pass  

   ### Scenario: [-ld given, but no source found](01_command_line.md): 
   - OK : Given new file `rules.txt` containing `Interfaces use is forbidden`  
   - OK : And   new directory `dir11`  
   - OK : When I run `./acc rules.txt -ld -I dir11`    
   - OK : Then I get   
   - OK : And  the exit code is `0`  
   - OK : And there is no error output  
   - [X] scenario   [-ld given, but no source found](01_command_line.md) pass  

   ### Scenario: [src found, but nothing to do with it](01_command_line.md): 
   - OK : Given directory `dir12`  
   - OK : And   file `dir12/src.adb`  
   - OK : When I run `./acc -I dir12`      
   - OK : Then the output contains  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [src found, but nothing to do with it](01_command_line.md) pass  

   ### Scenario: [rules file found, but nothing to do with it](01_command_line.md): 
   - OK : When I run `./acc rules.txt`   
   - OK : Then the output contains  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [rules file found, but nothing to do with it](01_command_line.md) pass  

   ### Scenario: [template creation when there's already one](01_command_line.md): 
   - OK : Given I run `./acc -ct` Successfully  
   - OK : When running once more `./acc --create_template`    
   - OK : Then I get   
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [template creation when there's already one](01_command_line.md) pass  

   ### Scenario: [-ar without rule](01_command_line.md): 
   - OK : When I run `./acc -ar`  or  `./acc --append_rule`    
   - OK : Then I get   
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [-ar without rule](01_command_line.md) pass  

   ### Scenario: [warnings are treated as errors when -We or --Warnings=error is set 1/2](01_command_line.md): 
   - OK : Given the new directory `dir9`  
   - OK : When I run `./acc -lf -We -I dir9`  
   - OK : Then I get `Warning : Cannot list files, no sources found to analyze`  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [warnings are treated as errors when -We or --Warnings=error is set 1/2](01_command_line.md) pass  

   ### Scenario: [warnings are treated as errors when -We or --Warnings=error is set 2/2](01_command_line.md): 
   - OK : Given the new directory `dir9`  
   - OK : When I run `./acc -lf --Warnings=error -I dir9`  
   - OK : Then I get `Warning : Cannot list files, no sources found to analyze`  
   - OK : And  the exit code is `1`  
   - OK : And there is no error output  
   - [X] scenario   [warnings are treated as errors when -We or --Warnings=error is set 2/2](01_command_line.md) pass  

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
   - OK : And   there is no dir `dir2`  
   - OK : And   there is no dir `dir3`  
   - [X] background [](02_source_list.md) pass  

   ### Scenario: [Recursive file identification test](02_source_list.md): 
   - OK : Given the `./dira/dira1/` dir  
   - OK : And   the `./dirb/dirb1/` dir  
   - OK : And   the file `./dira/a.ads`  
   - OK : And   the file `./dirb/b.ads`      
   - OK : And   the file `./dirb/dirb1/c.ads`      
   - OK : And   the file `./dira/dira1/c-d.ads`      
   - OK : When I run `./acc -I dira -Ir dirb --list_files`    
   - OK : Then the output is   
   - [X] scenario   [Recursive file identification test](02_source_list.md) pass  

   ### Background: [](02_source_list.md): 
   - OK : Given there is no dir `dir1`   
   - OK : And   there is no dir `dir2`  
   - OK : And   there is no dir `dir3`  
   - [X] background [](02_source_list.md) pass  

   ### Scenario: [Non recursive file identification test 1/2](02_source_list.md): 
   - OK : Given the new `./dir1/` dir  
   - OK : And   the new `./dir2/` dir  
   - OK : And   the new `./dir3/` dir  
   - OK : And   the file `./dir3/c-d.ads`  
   - OK : And   the file `./dir3/c.ads`      
   - OK : And   the file `./dir1/a.ads`      
   - OK : And   the file `./dir1/a.adb`      
   - OK : And   the file `./dir2/b.ads`      
   - OK : When I run `./acc -I dir1 -I dir2 -I dir3 -lf`    
   - OK : Then the output is   
   - [X] scenario   [Non recursive file identification test 1/2](02_source_list.md) pass  

   ### Background: [](02_source_list.md): 
   - OK : Given there is no dir `dir1`   
   - OK : And   there is no dir `dir2`  
   - OK : And   there is no dir `dir3`  
   - [X] background [](02_source_list.md) pass  

   ### Scenario: [Non recursive file identification test 2/2](02_source_list.md): 
   - OK : Given the new `./dir1/` dir  
   - OK : And   the new `./dir2/` dir  
   - OK : And   the new `./dir3/` dir  
   - OK : And   the file `./dir3/c-d.ads`  
   - OK : And   the file `./dir3/c.ads`      
   - OK : And   the file `./dir1/a.ads`      
   - OK : And   the file `./dir1/a.adb`      
   - OK : And   the file `./dir2/b.ads`      
   - OK : When I run `./acc -I dir1 -I dir2 -I dir3 --list_files`    
   - OK : Then the output is   
   - [X] scenario   [Non recursive file identification test 2/2](02_source_list.md) pass  


# Document: [03_dependency_list.md](03_dependency_list.md)  
  ## Feature: Dependencies identification simple test suite  
   ### Scenario: [Simple test](03_dependency_list.md): 
   - OK : Given the `./dir1/` dir  
   - OK : And   the file `./dir1/a.ads`  
   - OK : And   the file `./dir1/a.adb`      
   - OK : And   the file `./dir1/b.adb`      
   - OK : And   the file `./dir1/c.ads`      
   - OK : And   the file `./dir1/c-d.adb`      
   - OK : When I run `./acc -I dir1 --list_dependencies`  
   - OK : Then the output is (ignoring order)   
   - [X] scenario   [Simple test](03_dependency_list.md) pass  

   ### Scenario: [Source with weird formatted withed unit](03_dependency_list.md): 
   - OK : Given the new file `./dir1/c-d.adb`      
   - OK : When I run `./acc -I dir1 --list_dependencies`  
   - OK : Then the output is (ignoring order)  
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
   - OK : And   I run `./create_pkg Browser       spec -in dirB -with UI_Component` Successfully  
   - OK : And   I run `./create_pkg Rasterizer    spec -in dirB -with Transcoder` Successfully  
   - OK : And   I run `./create_pkg UI_Component  spec -in dirB -with Bridge -with Renderer` Successfully  
   - OK : And   I run `./create_pkg Transcoder    spec -in dirB -with Bridge -with Renderer` Successfully  
   - OK : And   I run `./create_pkg Bridge        spec -in dirB -with GVT -with SVGDOM` Successfully  
   - OK : And   I run `./create_pkg Renderer      spec -in dirB -with GVT` Successfully  
   - OK : And   I run `./create_pkg GVT           spec -in dirB` Successfully  
   - OK : And   I run `./create_pkg SVGDOM        spec -in dirB -with SVG_Parser` Successfully  
   - OK : And   I run `./create_pkg SVG_Parser    spec -in dirB` Successfully  
   - OK : And   I run `./create_pkg SVG_Generator spec -in dirB -with SVGDOM` Successfully  
   - OK : When I run `./acc -q -I dirB rules.B`  
   - OK : Then I get no output  
   - OK : And  I get no error  
   - [X] scenario   [Sanity test, the Batik project architecture](05_layer_rule.md) pass  

   ### Scenario: [Illegal upward dependency](05_layer_rule.md): 
   - OK : Given the file `rules.2`  
   - OK : And   I run `./create_pkg P1 spec  -in dir2 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir2 -with P3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir2` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir2 -with P5` Successfully  
   - OK : And   I run `./create_pkg P5 spec  -in dir2` Successfully  
   - OK : When I run `./acc --quiet -I dir2 rules.2`  
   - OK : Then I get  
   - [X] scenario   [Illegal upward dependency](05_layer_rule.md) pass  

   ### Scenario: [Layer bridging](05_layer_rule.md): 
   - OK : Given the file `rules.3`  
   - OK : And   I run `./create_pkg P1 spec  -in dir3a -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir3a` Successfully  
   - OK : And   I run `./create_pkg P2 body  -in dir3a -with P3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir3b` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir3b` Successfully  
   - OK : And   I run `./create_pkg P6 spec  -in dir3c` Successfully  
   - OK : And   I run `./create_pkg P6 body  -in dir3c -with P4` Successfully  
   - OK : When I run `./acc -I dir3a -I dir3b -I dir3c rules.3`  
   - OK : Then I get  
   - [X] scenario   [Layer bridging](05_layer_rule.md) pass  

   ### Scenario: [Using a package that is neither in the same layer, nor in the visible layer](05_layer_rule.md): 
   - OK : Given the file `rules.4`  
   - OK : And   I run `./create_pkg P1 spec  -in dir4 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir4 -with P3 -with P4 -with P7` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir4` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir4` Successfully  
   - OK : And   I run `./create_pkg P7 spec  -in dir4` Successfully  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then I get  
   - [X] scenario   [Using a package that is neither in the same layer, nor in the visible layer](05_layer_rule.md) pass  

   ### Scenario: [Base normal situation 1/2](05_layer_rule.md): 
   - OK : Given the file `rules.1`  
   - OK : And   the `./dir1/` dir  
   - OK : And   I run `./create_pkg P1 spec  -in dir1 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir1 -with P3` Successfully  
   - OK : And   I run `./create_pkg P2 body  -in dir1 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir1` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir1` Successfully  
   - OK : When I run `./acc -q -I dir1 rules.1`  
   - OK : Then I get no output  
   - OK : And  I get no error  
   - [X] scenario   [Base normal situation 1/2](05_layer_rule.md) pass  

   ### Scenario: [Base normal situation 2/2](05_layer_rule.md): 
   - OK : Given the file `rules.1`  
   - OK : And   the `./dir1/` dir  
   - OK : And   I run `./create_pkg P1 spec  -in dir1 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir1 -with P3` Successfully  
   - OK : And   I run `./create_pkg P2 body  -in dir1 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir1` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir1` Successfully  
   - OK : When I run `./acc --quiet -I dir1 rules.1`  
   - OK : Then I get no output  
   - OK : And  I get no error  
   - [X] scenario   [Base normal situation 2/2](05_layer_rule.md) pass  


# Document: [06_child_packages.md](06_child_packages.md)  
  ## Feature: Child packages test suite  
   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Rules OK test, no output expected](06_child_packages.md): 
   - OK : Given I run `./create_pkg GuI.P1 spec  -in dir1 -with GUI.P2` Successfully  
   - OK : And   I run `./create_pkg GUI.P2 spec  -in dir1 -with DB.p3 -with DB.P4` Successfully  
   - OK : And   I run `./create_pkg Db.P3  spec  -in dir1` Successfully  
   - OK : And   I run `./create_pkg DB.P4  spec  -in dir1` Successfully  
   - OK : When I run `./acc -q -I dir1 rules.txt`  
   - OK : Then I get no error  
   - OK : And  I get no output  
   - [X] scenario   [Rules OK test, no output expected](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Reverse dependency test](06_child_packages.md): 
   - OK : Given I run `./create_pkg GUI.P1 spec  -in dir2 -with GUI.P2` Successfully  
   - OK : And   I run `./create_pkg GUI.P2 spec  -in dir2 -with DB.P3 -with DB.P4` Successfully  
   - OK : And   I run `./create_pkg DB.P3  spec  -in dir2` Successfully  
   - OK : And   I run `./create_pkg DB.P4  spec  -in dir2` Successfully  
   - OK : And   I run `./create_pkg DB.P4  body  -in dir2 -with GUI.P5` Successfully  
   - OK : And   I run `./create_pkg GUI.P5 spec  -in dir2` Successfully  
   - OK : When I run `./acc -q -I dir2 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Reverse dependency test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Layer bridging test](06_child_packages.md): 
   - OK : Given I run `./create_pkg GUI.P1 spec  -in dir3 -with GUI.P2` Successfully  
   - OK : And   I run `./create_pkg GUI.P2 spec  -in dir3 -with DB.P3 -with DB.P4` Successfully  
   - OK : And   I run `./create_pkg DB.P3  spec  -in dir3` Successfully  
   - OK : And   I run `./create_pkg DB.P4  spec  -in dir3` Successfully  
   - OK : And   I run `./create_pkg P6     spec  -in dir3 -with DB.P4` Successfully  
   - OK : When I run `./acc -I dir3 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Layer bridging test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Undescribed dependency test](06_child_packages.md): 
   - OK : Given I run `./create_pkg GUI.P1 spec  -in dir4 -with GUI.P2` Successfully  
   - OK : And   I run `./create_pkg GUI.P2 spec  -in dir4 -with DB.P3 -with DB.P4 -with P7` Successfully  
   - OK : And   I run `./create_pkg DB.P3  spec  -in dir4` Successfully  
   - OK : And   I run `./create_pkg DB.P4  spec  -in dir4` Successfully  
   - OK : And   I run `./create_pkg P7     spec  -in dir4` Successfully  
   - OK : When I run `./acc -I dir4 rules.txt`  
   - OK : Then I get  
   - [X] scenario   [Undescribed dependency test](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Packages in the same layer may with them self](06_child_packages.md): 
   - OK : Given I run `./create_pkg GUI.P1 spec  -in dir5` Successfully  
   - OK : And   I run `./create_pkg GUI.P2 spec  -in dir5 -with GUI.P1` Successfully  
   - OK : And   I run `./create_pkg DB.P3  spec  -in dir5` Successfully  
   - OK : And   I run `./create_pkg DB.P4  spec  -in dir5 -with DB.P3` Successfully  
   - OK : When I run `./acc -I dir5 rules.txt`  
   - OK : Then I get no error  
   - OK : And  I get no output  
   - [X] scenario   [Packages in the same layer may with them self](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg](06_child_packages.md): 
   - OK : Given I run `./create_pkg GUI.P1 spec  -in dir6 -with DB.P1` Successfully  
   - OK : And   I run `./create_pkg GUIP2  spec  -in dir6 -with DB.P1` Successfully  
   - OK : And   I run `./create_pkg DB.P1  spec  -in dir6` Successfully  
   - OK : And   I run `./create_pkg DBP2   spec  -in dir6 -with DB.P1` Successfully  
   - OK : When I run `./acc -I dir6 rules.txt`  
   - OK : Then I get (ignoring order)  
   - [X] scenario   [GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg](06_child_packages.md) pass  

   ### Background: [](06_child_packages.md): 
   - OK : Given the file `rules.txt`  
   - [X] background [](06_child_packages.md) pass  

   ### Scenario: [Forbidding Interfaces but allowing Interfaces.C](06_child_packages.md): 
   - OK : Given the file `rules7.txt`  
   - OK : And   I run `./create_pkg Interfaces   spec -in dir7` Successfully  
   - OK : And   I run `./create_pkg Interfaces.C spec -in dir7` Successfully  
   - OK : And   I run `./create_pkg P1           spec -in dir7 -with Interfaces` Successfully  
   - OK : And   I run `./create_pkg P2           spec -in dir7 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P3           spec -in dir7 -with Interfaces.Java` Successfully  
   - OK : When I run `./acc -I dir7 rules7.txt`  
   - OK : Then I get (ignoring order)  
   - [X] scenario   [Forbidding Interfaces but allowing Interfaces.C](06_child_packages.md) pass  


# Document: [07_rules_files_syntax.md](07_rules_files_syntax.md)  
  ## Feature: Rules file syntax test suite  
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

   ### Scenario: [Reference file 1/2](07_rules_files_syntax.md): 
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc -lr rules1.txt`    
   - OK : then I get  
   - [X] scenario   [Reference file 1/2](07_rules_files_syntax.md) pass  

   ### Scenario: [Reference file 2/2](07_rules_files_syntax.md): 
   - OK : Given the file `rules1.txt`  
   - OK : When I run `./acc --list_rules rules1.txt`    
   - OK : then I get  
   - [X] scenario   [Reference file 2/2](07_rules_files_syntax.md) pass  


# Document: [08_globbing_characters.md](08_globbing_characters.md)  
  ## Feature: Globbing Character test suite  
   ### Scenario: [rules test](08_globbing_characters.md): 
   - OK : Given the file `rules.1`  
   - OK : And   I run `./create_pkg P1 spec -in dir1 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec  -in dir1 -with P3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec  -in dir1 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P4 spec  -in dir1 -with Interfaces.C.Strings` Successfully  
   - OK : When I run `./acc -I dir1 rules.1`   
   - OK : Then there is no error  
   - OK : And  there is no output  
   - [X] scenario   [rules test](08_globbing_characters.md) pass  

   ### Scenario: [illegal use of Interfaces from Application_Layer](08_globbing_characters.md): 
   - OK : Given the file `rules.2`  
   - OK : And   I run `./create_pkg P1 spec -in dir2 -with P2 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir2 -with P3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir2 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir2 -with Interfaces.C.Strings` Successfully  
   - OK : When I run `./acc -I dir2 rules.2`   
   - OK : Then I get  
   - [X] scenario   [illegal use of Interfaces from Application_Layer](08_globbing_characters.md) pass  


# Document: [09_gtkada.md](09_gtkada.md)  
  ## Feature: GtkAda test suite  
   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : And   I run `unzip -q ../docs/tests_results/09_gtkada/gtkada-master.zip` Successfully  
   - OK : And   I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [File Identification](09_gtkada.md): 
   - OK : Given I run `find gtkada-master -name "*.ad[sb]"` Successfully  
   - OK : When I run `./acc -q -lf -r -I gtkada-master` Successfully  
   - OK : Then I get file `../docs/tests_results/09_gtkada/expected_output.1` (ignoring order)  
   - [X] scenario   [File Identification](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : And   I run `unzip -q ../docs/tests_results/09_gtkada/gtkada-master.zip` Successfully  
   - OK : And   I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [Unit Identification](09_gtkada.md): 
   - OK : When I run `./acc -ld -r -I gtkada-master` Successfully  
   - OK : Then I get file `../docs/tests_results/09_gtkada/expected_output.2` (ignoring order)  
   - [X] scenario   [Unit Identification](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : And   I run `unzip -q ../docs/tests_results/09_gtkada/gtkada-master.zip` Successfully  
   - OK : And   I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [A realistic GtkAda description file](09_gtkada.md): 
   - OK : Given there is a file `../docs/tests_results/09_gtkada/GtkAda.ac`  
   - OK : When I run `./acc ../docs/tests_results/09_gtkada/GtkAda.ac -r -I gtkada-master` Successfully  
   - OK : Then I get file `../docs/tests_results/09_gtkada/expected_output.3` (ignoring order)  
   - [X] scenario   [A realistic GtkAda description file](09_gtkada.md) pass  

   ### Background: [](09_gtkada.md): 
   - OK : Given there is no dir `gtkada-master`  
   - OK : And   I run `unzip -q ../docs/tests_results/09_gtkada/gtkada-master.zip` Successfully  
   - OK : And   I run `find gtkada-master -name "*.[ch]" -delete` Successfully  
   - [X] background [](09_gtkada.md) pass  

   ### Scenario: [Another realistic GtkAda description file](09_gtkada.md): 
   - OK : Given there is a file `../docs/tests_results/09_gtkada/GtkAda2.ac`  
   - OK : When I run `./acc ../docs/tests_results/09_gtkada/GtkAda2.ac -q -r -I gtkada-master` Successfully  
   - OK : Then I get file `../docs/tests_results/09_gtkada/expected_output.4` (ignoring order)  
   - [X] scenario   [Another realistic GtkAda description file](09_gtkada.md) pass  


# Document: [10_use_rule.md](10_use_rule.md)  
  ## Feature: Use rules test suite  
   ### Scenario: [May_Use rule, code compliant, no output expected](10_use_rule.md): 
   - OK : Given there is no `dir1` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir1 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir1 -with P3` Successfully  
   - OK : And   I run `./create_pkg P2 body -in dir1 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir1` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir1` Successfully  
   - OK : And   the file `rules.1`  
   - OK : When I run `./acc -q -I dir1 rules.1`  
   - OK : Then I get no output  
   - [X] scenario   [May_Use rule, code compliant, no output expected](10_use_rule.md) pass  

   ### Scenario: [Using a unit from a non allowed unit](10_use_rule.md): 
   - OK : Given there is no `dir2` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir2 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir2 -with P3` Successfully  
   - OK : And   I run `./create_pkg P2 body -in dir2 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir2` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir2` Successfully  
   - OK : And   I run `./create_pkg P1 body -in dir2 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P4 body -in dir2 -with Interfaces.C` Successfully  
   - OK : And   the new file `rules.1`  
   - OK : When I run `./acc -q -I dir2 rules.1`  
   - OK : Then output is  
   - [X] scenario   [Using a unit from a non allowed unit](10_use_rule.md) pass  

   ### Scenario: [Forbidden use test](10_use_rule.md): 
   - OK : Given there is no `dir3` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir3 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir3 -with P3` Successfully  
   - OK : And   I run `./create_pkg P2 body -in dir3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir3` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir3` Successfully  
   - OK : And   the file `rules.3`  
   - OK : When I run `./acc -q -I dir3 rules.3`  
   - OK : Then output is  
   - [X] scenario   [Forbidden use test](10_use_rule.md) pass  

   ### Scenario: [Allowing use of an environnement package](10_use_rule.md): 
   - OK : Given there is no `dir4` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir4 -with P2` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir4 -with Containers.Generic_Sort` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir4` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir4` Successfully  
   - OK : And   the file `rules.4`  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then output is  
   - OK : Given the new file `rules.4`  
   - OK : When I run `./acc -I dir4 rules.4`  
   - OK : Then I get no output  
   - [X] scenario   [Allowing use of an environnement package](10_use_rule.md) pass  

   ### Scenario: [Cumulative only ... may use X rules](10_use_rule.md): 
   - OK : Given there is no `dir5` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir5 -with Interfaces.C` Successfully  
   - OK : And   the file `rules.5`  
   - OK : When I run `./acc -I dir5 rules.5`  
   - OK : Then output is  
   - [X] scenario   [Cumulative only ... may use X rules](10_use_rule.md) pass  

   ### Scenario: [Combining Allowed and Forbidden](10_use_rule.md): 
   - OK : Given there is no `dir6` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir6 -with Interfaces` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir6 -with Interfaces.C` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir6 -with Interfaces.Java` Successfully  
   - OK : And   the file `rules.6`  
   - OK : When I run `./acc -I dir6 rules.6`  
   - OK : Then output is (ignoring order)  
   - OK : Given the new file `rules.6b`  
   - OK : When I run `./acc -I dir6 rules.6b`  
   - OK : Then I get no output  
   - [X] scenario   [Combining Allowed and Forbidden](10_use_rule.md) pass  

   ### Scenario: [X may use Unit List rules](10_use_rule.md): 
   - OK : Given there is no `dir7` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir7 -with P2 -with P3 -with P4` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir7 -with P1` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir7 -with P1` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir7 -with P1` Successfully  
   - OK : And   the file `rules.7`  
   - OK : When I run `./acc -I dir7 rules.7`  
   - OK : Then output is (ignoring order)  
   - [X] scenario   [X may use Unit List rules](10_use_rule.md) pass  

   ### Scenario: [only X may use Unit List rules](10_use_rule.md): 
   - OK : Given there is no `dir8` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir8 -with P2 -with P3` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir8 -with P2 -with P3` Successfully  
   - OK : And   the file `rules.8`  
   - OK : When I run `./acc -I dir8 rules.8`  
   - OK : Then output is  
   - [X] scenario   [only X may use Unit List rules](10_use_rule.md) pass  

   ### Scenario: [Appending rules 1/2](10_use_rule.md): 
   - OK : Given there is no `dir9` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir9` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir9 -with P2` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : And   I run `./create_pkg Bus spec -in dir9` Successfully  
   - OK : And   I run `./create_pkg IO spec -in dir9` Successfully  
   - OK : And   the file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 -ar "only P1 may use IO" rules.9`  
   - OK : Then output is  
   - OK : Given the new file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9`  
   - OK : Then output is  
   - [X] scenario   [Appending rules 1/2](10_use_rule.md) pass  

   ### Scenario: [Appending rules 2/2](10_use_rule.md): 
   - OK : Given there is no `dir9` directory  
   - OK : And   I run `./create_pkg P1 spec -in dir9` Successfully  
   - OK : And   I run `./create_pkg P2 spec -in dir9 -with P2` Successfully  
   - OK : And   I run `./create_pkg P3 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : And   I run `./create_pkg P4 spec -in dir9 -with P2 -with P3` Successfully  
   - OK : And   I run `./create_pkg Bus spec -in dir9` Successfully  
   - OK : And   I run `./create_pkg IO spec -in dir9` Successfully  
   - OK : And   the file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 --append_rule "only P1 may use IO" rules.9`  
   - OK : Then output is  
   - OK : Given the new file `rules.9`  
   - OK : When I run `./acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9`  
   - OK : Then output is  
   - [X] scenario   [Appending rules 2/2](10_use_rule.md) pass  


# Document: [11_batik.md](11_batik.md)  
  ## Feature: Batik test suite  
   ### Scenario: [--list_file test](11_batik.md): 
   - OK : Given there is no `batik-1.9` directory  
   - OK : And   I run `tar -xf ../docs/tests_results/11_batik/batik-src-1.9.tar.gz` Successfully  
   - OK : When I run `./acc -lf -Ir ./batik-1.9`  
   - OK : Then I get file `../docs/tests_results/11_batik/expected_output.1` (ignoring order)  
   - [X] scenario   [--list_file test](11_batik.md) pass  

   ### Scenario: [public class](11_batik.md): 
   - OK : Given the new directory `dir2`  
   - OK : And   I run `tar -xf ../docs/tests_results/11_batik/batik-src-1.9.tar.gz` Successfully  
   - OK : And   I run `cp ./batik-1.9/contrib/jsvg/JSVG.java dir2` Successfully  
   - OK : When I run `./acc -ld -I dir2`  
   - OK : Then I get file `../docs/tests_results/11_batik/expected_output.2`  
   - [X] scenario   [public class](11_batik.md) pass  

   ### Scenario: [public interface class](11_batik.md): 
   - OK : Given the new directory `dir3`  
   - OK : And   I run `tar -xf ../docs/tests_results/11_batik/batik-src-1.9.tar.gz` Successfully  
   - OK : And   I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3` Successfully  
   - OK : When I run `./acc -ld -I dir3`  
   - OK : Then I get file `../docs/tests_results/11_batik/expected_output.3`  
   - [X] scenario   [public interface class](11_batik.md) pass  

   ### Scenario: [no import](11_batik.md): 
   - OK : Given the new directory `dir4`  
   - OK : And   I run `tar -xf ../docs/tests_results/11_batik/batik-src-1.9.tar.gz` Successfully  
   - OK : And   I run `cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4` Successfully  
   - OK : When I run `./acc -ld -I dir4`  
   - OK : Then I get file `../docs/tests_results/11_batik/expected_output.4`  
   - [X] scenario   [no import](11_batik.md) pass  

   ### Scenario: [no package](11_batik.md): 
   - OK : Given the new directory `dir5`  
   - OK : And   the file `dir5/MyClass.java`  
   - OK : When I run `./acc -ld -I dir5`  
   - OK : Then I get file `../docs/tests_results/11_batik/expected_output.5`  
   - [X] scenario   [no package](11_batik.md) pass  

   ### Scenario: [public abstract class](11_batik.md): 
   - O