## Feature: Rules vs sources coverage test suite

### Scenario: Warnings on units appearing in rules file and not related to any source

- Given there is no `dir1` directory
- Given I run `./create_pkg P1 spec -in dir1` Successfully
- Given I run `./create_pkg P2 spec -in dir1` Successfully
- Given I run `./create_pkg P5 spec -in dir1` Successfully
- Given the file `test1.ac`
```
Framework contains P1, P2 and P3
-- no P3 src, should raise a warning

P4 may use P1 
-- no P4 src, should raise a warning

Java.IO use is forbidden
-- no Java.IO sources, but should not raise a warning

P5 may use Framework
-- Framework match no sources, but should not raise warning
-- as it's a Component.

```
- When I run `./acc test1.ac -I ./dir1`
- Then output is
```
Warning : test1.ac:4: P3 do not match any compilation unit
Warning : test1.ac:7: P4 do not match any compilation unit
```

### Scenario: Non covered sources

- Given there is no `dir2` directory
- Given I run `./create_pkg P2 spec -in dir2` Successfully
- Given I run `./create_pkg P3 spec -in dir2` Successfully
- Given I run `./create_pkg P4 spec -in dir2` Successfully
- Given I run `./create_pkg P5 spec -in dir2` Successfully
- Given I run `./create_pkg P1.X spec -in dir2` Successfully
- Given I run `./create_pkg Y.P1 spec -in dir2` Successfully
- Given I run `./create_pkg Framework.Utilities spec -in dir2` Successfully
- Given I run `./create_pkg Framework_Utilities spec -in dir2` Successfully
- Given I run `./create_pkg Java.Awt spec -in dir2` Successfully
- Given I run `./create_pkg Java spec -in dir2` Successfully
- Given the file `test2.ac`
```
Framework contains P1, P2 and P3
-- no P3 src, should raise a warning

P4 may use P1 
-- no P4 src, should raise a warning

Java.IO use is forbidden
-- no Java.IO sources, but should not raise a warning

P5 may use Framework
-- Framework match no sources, but should not raise warning
-- as it's a Component.

```
- When I run `./acc -lnc test2.ac -I ./dir2`
- Then output is (unordered)
```
Framework_Utilities
Java
Java.Awt
Y.P1
```

### Scenario: Case insensitivity of Is_A_Component function (non reg)

- Given there is no `dir3` directory
- Given I run `./create_pkg P1 spec -in dir3` Successfully
- Given the file `test3.ac`
```
-- Check correction of a bug due to case sensitivity of Unit_Name default "="

SYSTEM contains org.springframework.samples.petclinic.system.WelcomeController

Org.SpringFramework may use Model, system
-- Model is not a component => Warning because of no Matching sources
-- system is a component    => no warning expected system should be identified as
--                             the component declared in uppercase.
```
- When I run `./acc test3.ac -I dir3`
- Then output is
```
Warning : test3.ac:8: Org.SpringFramework do not match any compilation unit
Warning : test3.ac:5: org.springframework.samples.petclinic.system.WelcomeController do not match any compilation unit
Warning : test3.ac:8: Model do not match any compilation unit
```
