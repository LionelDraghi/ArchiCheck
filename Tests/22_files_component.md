## Feature: Adding compilation units through files to a Component

### Scenario: Sources identification through rules file (no -I on command line)

- Given there is no `dir1` directory
- Given there is no `dir2` directory
- Given the new directory `dir1`
- Given I run `./create_pkg B body -in dir2` Successfully
- Given the file `dir1/a.ads`
```ada
with B;
package A is
end A;
```
- Given the file `dir1/a.adb`
```ada
with C;
package body A is
end A;
```
- Given the file `dir2/c.ads`
```ada
with B, F;
package C is
end C;
```
- When I run `./acc -I dir1 --list_dependencies`
- Then output is (unordered)
```
A package body depends on C
A package spec depends on B
```

### Scenario: Source with weird formatted withed unit

- Given there is no `dir2` directory
- Given the new directory `dir1`
- Given I run `./create_pkg B body -in dir2` Successfully
- Given the file `dir2/a.ads`
```ada
with B;
package A is
end A;
```
- Given the file `dir2/a.adb`
```ada
with C;
package body A is
end A;
```
- Given the file `dir2/b.adb`
```ada
package body B is
end B;
```
- Given the file `dir2/c.ads`
```ada
with B, F;
package C is
end C;
```
- Given the file `dir2/c-d.adb`
```ada
with   
     E, -- xyz
  F, G;
function C .D is
   null;
end C . D ;
```
- When I run `./acc -I dir2 --list_dependencies`
- Then output is (unordered)
```
A package body depends on C
A package spec depends on B
C.D function body depends on E
C.D function body depends on F
C.D function body depends on G
C package spec depends on B
C package spec depends on F
```
