Fixme in current version:
-------------------------

Location | Text
---------|-----
src/archicheck-lang-ada_processor.adb:189|           (case Token_ID is -- Fixme: déclarer un sous-type de Ada_Token
src/archicheck-lang-c_processor.adb:127|                     -- Fixme: Only one unit name per import statement in C??
src/archicheck-lang.adb:67|            -- Fixme: rename Print
src/archicheck-lang.ads:32|                           Recursive : in Boolean); --** Fixme: to be renamed
src/archicheck-rules-check.adb:65|               -- Fixme: il faut que Is_Allowed et les autres retourne un
src/archicheck-rules-check_unrelated_rules_units.adb:59|   -- Fixme: Uggly solution to manage a unique item list
src/archicheck-rules-dump_unrelated_compilation_units.adb:17|   -- Fixme: Uggly solution to manage a unique item list
src/archicheck-rules-parser.adb:448|         -- Fixme: put_line à déplacer dans rules, ou dans units
src/archicheck-rules.adb:110|      -- Fixme: ajouter une référence à la règle en param out
src/archicheck.ads:22|-- Fixme: obsolete description!
src/backup/archicheck-dependencies.ads:49|--        -- Fixme:
Tests/08_Globbing_Characters/Makefile:41|	@ ## Fixme: pas encore assez robuste pour ce test!! @ ../../obj/archicheck -I dir1 rules.1 > output.1
Tests/08_Globbing_Characters/Makefile:42|	@ ## Fixme: pas encore assez robuste pour ce test!! ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"
Tests/08_Globbing_Characters/Makefile:78|	@ ## Fixme: pas encore assez robuste pour ce test!! @ ../../obj/archicheck -I dir2 rules.2 > output.2
Tests/08_Globbing_Characters/Makefile:79|	@ ## Fixme: pas encore assez robuste pour ce test!! ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"
Tests/09_GtkAda/Makefile:105|	@ # Fixme: -q is set to avoid warnings and focus on error till code is fixed
Tests/10_Use_Rule/Makefile:191|	@ ## Fixme: @ test ! `../../obj/archicheck -I dir5 rules.5b > output.5b`
Tests/15_Precedences_Rules/rules2.txt:4|-- Fixme: and what if declared the other way round?
Tests/15_Precedences_Rules/testrec.md:40|-- Fixme: and what if declared the other way round?
