## Feature: Component processing in rules unit test

### Scenario: Layer component

- Given there is no `src` directory
- Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully
- Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully
- Given I run `./create_pkg LC spec -in src` Successfully
- Given the file `rules1a.txt`
```
LA is a Layer over LB
LB is a Layer over LC
```
- When I run `./acc rules1a.txt -I src`
- Then output is
```
Warning : src/lb-y.adb:1: LB.Y (in LB layer) uses Ada.Containers that is neither in the same layer, nor in the lower LC layer
Warning : src/lb-y.adb:2: LB.Y (in LB layer) uses Interfaces.C that is neither in the same layer, nor in the lower LC layer
```

- Given there is no `src` directory
- Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully
- Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully
- Given I run `./create_pkg LC spec -in src` Successfully
- Given the file `rules1b.txt`
```
LA is a Layer over LB
LB is a Layer over LC

LC contains Interfaces.C and Ada

```
- When I run `./acc rules1b.txt -I src`
- Then I get no output

### Scenario: Env component allowed

- Given there is no `src` directory
- Given I run `./create_pkg LA.X spec -in src -with LB.Y` Successfully
- Given I run `./create_pkg LB.Y body -in src -with Ada.Containers -with Interfaces.C` Successfully
- Given I run `./create_pkg LC spec -in src` Successfully
- Given the file `rules2.txt`
```
LA is a Layer over LB
LB is a Layer over LC

Env contains Interfaces.C and Ada
Env use is allowed
```
- When I run `./acc rules2.txt -I src`
- Then I get no output

### Scenario: Trying to include a unit in more components

- Given there is no `src` directory
- Given I run `./create_pkg P1 spec -in src` Successfully
- Given I run `./create_pkg P2 spec -in src` Successfully
- Given I run `./create_pkg P3 spec -in src` Successfully
- Given I run `./create_pkg P4 spec -in src` Successfully
- Given the file `rules3.txt`
```
-- Two simple components :
X contains P1 and P2
Y contains P3 and P4
-- P1 to P4 are existing compilation units

-- A component of components
Z contains X and Y

-- till now, OK.

-- and now let's try to add P1 (that belong to X) to another components :
Y contains P1
Z contains P1
```
- When I run `./acc rules3.txt -I src`
- Then output is
```
Error : rules3.txt:12: P1 already in X (cf. rules3.txt:2: ), can't be added to Y
Error : rules3.txt:13: P1 already in X (cf. rules3.txt:2: ), can't be added to Z
```

### Scenario: Test on Components embedding components embedding components...

- Given there is no `dir4` directory
- Given I run `./create_pkg P1 spec -in dir4 -with Ada.Containers -with Interfaces.C` Successfully
- Given the file `rules4.txt`
```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden
```
- When I run `./acc rules4.txt -I dir4`
- Then output is
```
Error : dir4/p1.ads:2: Interfaces.C use is forbidden
```

- Given the new file `rules4.txt`
```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden

Z may use Interfaces.C
```
- When I run `./acc rules4.txt -I dir4`
- Then I get no output

### Scenario: Test A B C example posted on fr.comp.lang.ada...

- Given there is no `dir5` directory
- Given I run `./create_pkg X.P1 spec -in dir5` Successfully
- Given I run `./create_pkg Y.P1 spec -in dir5` Successfully
- Given I run `./create_pkg Y.P2 spec -in dir5` Successfully
- Given I run `./create_pkg Z.P1 spec -in dir5` Successfully
- Given I run `./create_pkg Z.P2 spec -in dir5` Successfully
- Given I run `./create_pkg U spec -in dir5` Successfully
- Given I run `./create_pkg V spec -in dir5` Successfully
- Given the new file `dir5/y-p1.ads`
```ada
with X.P1;
package Y.P1 is
end Y.P1;
```
- Given the new file `dir5/z-p1.ads`
```ada
with X;
package Z.P1 is
end Z.P1;
```
- Given the new file `dir5/z-p2.ads`
```ada
with Y.P2;
package Z.P2 is
end Z.P2;
```
- Given the new file `dir5/u.ads`
```ada
with Z.P2;
package U is
end U;
```
- Given the new file `dir5/v.ads`
```ada
with Y.P1;
package V is
end V;
```
- Given the file `rules5.txt`
```
My_Layer contains X and Y
    
My_Layer is a layer over Z
```
- When I run `./acc rules5.txt -I dir5`
- Then output is
```
Error : dir5/z-p1.ads:1: Z.P1 is in Z layer, and so shall not use X in the upper My_Layer layer
Error : dir5/z-p2.ads:1: Z.P2 is in Z layer, and so shall not use Y.P2 in the upper My_Layer layer
Warning : dir5/u.ads:1: U is neither in My_Layer or Z layer, and so shall not directly use Z.P2 in the Z layer
```
