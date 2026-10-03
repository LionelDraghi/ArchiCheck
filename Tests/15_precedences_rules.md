## Feature: Precedence rules test suite

### Scenario: Declaration of a component already existing in code

- Given there is no `src` directory
- Given I run `./create_pkg LC.Z spec -in src` Successfully
- Given the file `src/la-x.ads`
```ada
with LB.Y;
package LA.X is
end LA.X;
```
- Given the file `src/lb-y.ads`
```ada
with Ada.Containers,
     Interfaces.C,
     Interfaces,
     Interfaces.Java,
     LC.Z,
     LA.X,
     LB.U;
package LB.Y is
end LB.Y;
```
- Given the file `rules1.txt`
```
LA is a Layer over LB
LB is a Layer over LC

LC contains Interfaces.C and Ada

```
- When I run `./acc rules1.txt -I src`
- Then output is
```
Warning : src/lb-y.ads:3: LB.Y (in LB layer) uses Interfaces that is neither in the same layer, nor in the lower LC layer
Warning : src/lb-y.ads:4: LB.Y (in LB layer) uses Interfaces.Java that is neither in the same layer, nor in the lower LC layer
Error : src/lb-y.ads:6: LB.Y is in LB layer, and so shall not use LA.X in the upper LA layer
```

### Scenario: Alowing a child of forbidden unit

- Given there is no `src` directory
- Given I run `./create_pkg LC.Z spec -in src` Successfully
- Given the file `src/la-x.ads`
```ada
with LB.Y;
package LA.X is
end LA.X;
```
- Given the file `src/lb-y.ads`
```ada
with Ada.Containers,
     Interfaces.C,
     Interfaces,
     Interfaces.Java,
     LC.Z,
     LA.X,
     LB.U;
package LB.Y is
end LB.Y;
```
- Given the file `rules2.txt`
```
Interfaces   use is forbidden
Interfaces.C use is allowed

-- Fixme: and what if declared the other way round?
```
- When I run `./acc rules2.txt -I src`
- Then output is
```
Error : src/lb-y.ads:3: Interfaces use is forbidden
Error : src/lb-y.ads:4: Interfaces.Java use is forbidden
```
