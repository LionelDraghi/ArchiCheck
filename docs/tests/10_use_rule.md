## Feature: Use rules test suite

### Scenario: May_Use rule, code compliant, no output expected

Code compliant with the rules, should be OK

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B

P2 may use P4
P3 may use P1
```

- Given there is no `dir1` directory
- Given I run `./create_pkg P1 spec -in dir1 -with P2` Successfully
- Given I run `./create_pkg P2 spec -in dir1 -with P3` Successfully
- Given I run `./create_pkg P2 body -in dir1 -with P4` Successfully
- Given I run `./create_pkg P3 spec -in dir1` Successfully
- Given I run `./create_pkg P4 spec -in dir1` Successfully
- Given the file `rules.1`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B

P2 may use P4
P3 may use P1
```
- When I run `./acc -q -I dir1 rules.1`
- Then output is empty

### Scenario: Using a unit from a non allowed unit

P4 body is using Interfaces.C : OK  
P1 body is using Interfaces.C : should complain

Expecting :

```
Error : dir2/p1.adb:2: Only Layer_B is allowed to use Interfaces*, P1 is not
```

- Given there is no `dir2` directory
- Given I run `./create_pkg P1 spec -in dir2 -with P2` Successfully
- Given I run `./create_pkg P2 spec -in dir2 -with P3` Successfully
- Given I run `./create_pkg P2 body -in dir2 -with P4` Successfully
- Given I run `./create_pkg P3 spec -in dir2` Successfully
- Given I run `./create_pkg P4 spec -in dir2` Successfully
- Given I run `./create_pkg P1 body -in dir2 -with Interfaces.C` Successfully
- Given I run `./create_pkg P4 body -in dir2 -with Interfaces.C` Successfully
- Given the file `rules.1`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B

P2 may use P4
P3 may use P1
```
- When I run `./acc -q -I dir2 rules.1`
- Then output is
```
Error : dir2/p1.adb:2: Only Layer_B is allowed to use Interfaces*, P1 is not
```

### Scenario: Forbidden use test

```
Application_Layer contains P1 and P2
Support_Layer contains P3 and P4
Application_Layer is a layer over Support_Layer

Interfaces use is forbidden
```

Expecting :

```
Error : dir3/p1.ads:2: Interfaces use is forbidden
Error : dir3/p2.ads:2: Interfaces use is forbidden
```

- Given there is no `dir3` directory
- Given I run `./create_pkg P1 spec -in dir3 -with P2` Successfully
- Given I run `./create_pkg P2 spec -in dir3 -with P3` Successfully
- Given I run `./create_pkg P2 body -in dir3 -with P4` Successfully
- Given I run `./create_pkg P3 spec -in dir3` Successfully
- Given I run `./create_pkg P4 spec -in dir3` Successfully
- Given the file `rules.3`
```
Application_Layer contains P1 and P2
Support_Layer contains P3 and P4
Application_Layer is a layer over Support_Layer

Interfaces use is forbidden
```
- When I run `./acc -q -I dir3 rules.3`
- Then output is
```
Error : dir3/p1.ads:2: Interfaces use is forbidden
Error : dir3/p2.ads:2: Interfaces use is forbidden
```

### Scenario: Allowing use of an environnement package

Rules :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
```

First test without the allowing rule : should complain

Expecting :

```
Warning : dir4/p2.ads:2: P2 (in Layer_A layer) uses Containers.Generic_Sort that is neither in the same layer, nor in the lower Layer_B layer
```

- Given there is no `dir4` directory
- Given I run `./create_pkg P1 spec -in dir4 -with P2` Successfully
- Given I run `./create_pkg P2 spec -in dir4 -with Containers.Generic_Sort` Successfully
- Given I run `./create_pkg P3 spec -in dir4` Successfully
- Given I run `./create_pkg P4 spec -in dir4` Successfully
- Given the file `rules.4`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
```
- When I run `./acc -I dir4 rules.4`
- Then output is
```
Warning : dir4/p2.ads:2: P2 (in Layer_A layer) uses Containers.Generic_Sort that is neither in the same layer, nor in the lower Layer_B layer
```

And now with the allowing rule :

Rules :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B

Containers use is allowed
```

No error expected.

- Given there is no `dir4` directory
- Given I run `./create_pkg P1 spec -in dir4 -with P2` Successfully
- Given I run `./create_pkg P2 spec -in dir4 -with Containers.Generic_Sort` Successfully
- Given I run `./create_pkg P3 spec -in dir4` Successfully
- Given I run `./create_pkg P4 spec -in dir4` Successfully
- Given the file `rules.4`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B

Containers use is allowed
```
- When I run `./acc -I dir4 rules.4`
- Then output is empty

### Scenario: Cumulative only ... may use X rules

P1 P2 and P3 are withing Interfaces.C

rules5.txt contains :

```
P1 may use Interfaces
P2 may use Interfaces
P3 may use Interfaces
only P4 may use Interfaces
```

When running :

> acc -I dir5 rules.5

Expected :

```
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P3 is not
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P2 is not
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P1 is not
```

- Given there is no `dir5` directory
- Given I run `./create_pkg P1 spec -in dir5 -with Interfaces.C` Successfully
- Given I run `./create_pkg P2 spec -in dir5 -with Interfaces.C` Successfully
- Given I run `./create_pkg P3 spec -in dir5 -with Interfaces.C` Successfully
- Given I run `./create_pkg P4 spec -in dir5 -with Interfaces.C` Successfully
- Given the file `rules.5`
```
P1 may use Interfaces
P2 may use Interfaces
P3 may use Interfaces
only P4 may use Interfaces
```
- When I run `./acc -I dir5 rules.5`
- Then output is
```
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P3 is not
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P2 is not
Error : dir5/p4.ads:2: Only P4 is allowed to use Interfaces, P1 is not
```

### Scenario: Combining Allowed and Forbidden

P1 P2 and P3 are withing Interfaces, Interfaces.C and Interfaces.Java

rules6.txt contains :

```
Interfaces use is forbidden
Interfaces.C use is allowed
```

When running :

> acc -I dir6 rules.6

Expected :

```
Error : dir6/p1.ads:2: Interfaces use is forbidden
Error : dir6/p3.ads:2: Interfaces.Java use is forbidden
```

- Given there is no `dir6` directory
- Given I run `./create_pkg P1 spec -in dir6 -with Interfaces` Successfully
- Given I run `./create_pkg P2 spec -in dir6 -with Interfaces.C` Successfully
- Given I run `./create_pkg P3 spec -in dir6 -with Interfaces.Java` Successfully
- Given the file `rules.6`
```
Interfaces use is forbidden
Interfaces.C use is allowed
```
- When I run `./acc -I dir6 rules.6`
- Then output is (unordered)
```
Error : dir6/p1.ads:2: Interfaces use is forbidden
Error : dir6/p3.ads:2: Interfaces.Java use is forbidden
```

And now inverting Allowed and Forbidden

rules6b.txt contains :

```
Interfaces use is allowed
Interfaces.C use is forbidden
```

When running :

> acc -I dir6 rules.6b

Expected :

```
Error : dir6/p2.ads:2: Interfaces.C use is forbidden
Error : dir6/p3.ads:2: Interfaces.Java use is forbidden
```

- Given there is no `dir6` directory
- Given I run `./create_pkg P1 spec -in dir6 -with Interfaces` Successfully
- Given I run `./create_pkg P2 spec -in dir6 -with Interfaces.C` Successfully
- Given I run `./create_pkg P3 spec -in dir6 -with Interfaces.Java` Successfully
- Given the file `rules.6b`
```
Interfaces use is allowed
Interfaces.C use is forbidden
```
- When I run `./acc -I dir6 rules.6b`
- Then output is
```
Error : dir6/p2.ads:2: Interfaces.C use is forbidden
Error : dir6/p3.ads:2: Interfaces.Java use is forbidden
```

### Scenario: X may use Unit List rules

P1 withing P2, P3, P4

rules7.txt contains :

```
only P1 may use P2 and P3 and P4
```

When running :

> acc -I dir7 rules.7

Expected

```
Error : dir7/p2.ads:2: Only P1 is allowed to use P2, P3 is not
Error : dir7/p2.ads:2: Only P1 is allowed to use P2, P4 is not
Error : dir7/p3.ads:2: Only P1 is allowed to use P3, P2 is not
Error : dir7/p3.ads:2: Only P1 is allowed to use P3, P4 is not
Error : dir7/p4.ads:2: Only P1 is allowed to use P4, P2 is not
Error : dir7/p4.ads:2: Only P1 is allowed to use P4, P3 is not
```

- Given there is no `dir7` directory
- Given I run `./create_pkg P1 spec -in dir7 -with P2 -with P3 -with P4` Successfully
- Given I run `./create_pkg P2 spec -in dir7 -with P1` Successfully
- Given I run `./create_pkg P3 spec -in dir7 -with P1` Successfully
- Given I run `./create_pkg P4 spec -in dir7 -with P1` Successfully
- Given the file `rules.7`
```
only P1 may use P2 and P3 and P4
```
- When I run `./acc -I dir7 rules.7`
- Then output is
```
Error : dir7/p2.ads:2: Only P1 is allowed to use P2, P3 is not
Error : dir7/p2.ads:2: Only P1 is allowed to use P2, P4 is not
Error : dir7/p3.ads:2: Only P1 is allowed to use P3, P2 is not
Error : dir7/p3.ads:2: Only P1 is allowed to use P3, P4 is not
Error : dir7/p4.ads:2: Only P1 is allowed to use P4, P2 is not
Error : dir7/p4.ads:2: Only P1 is allowed to use P4, P3 is not
```

### Scenario: only X may use Unit List rules

P1 withing P2 and P3
P4 withing P2 and P3

rules8.txt contains :

```
P1 may use P2 and P3
only P4 may use P2 and P3
```

When running :

> acc -I dir8 rules.8

Expected

```
Error : dir8/p4.ads:2: Only P4 is allowed to use P2, P1 is not
Error : dir8/p4.ads:3: Only P4 is allowed to use P3, P1 is not
```

- Given there is no `dir8` directory
- Given I run `./create_pkg P1 spec -in dir8 -with P2 -with P3` Successfully
- Given I run `./create_pkg P4 spec -in dir8 -with P2 -with P3` Successfully
- Given the file `rules.8`
```
P1 may use P2 and P3
only P4 may use P2 and P3
```
- When I run `./acc -I dir8 rules.8`
- Then output is
```
Error : dir8/p4.ads:2: Only P4 is allowed to use P2, P1 is not
Error : dir8/p4.ads:3: Only P4 is allowed to use P3, P1 is not
```

### Scenario: Appending rules

rules.9 contains :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
```

When running :

> acc -lr -I dir9 -ar "only P1 may use IO" rules.9

Expected

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
P1 may use IO
```

- Given there is no `dir9` directory
- Given I run `./create_pkg P1 spec -in dir9` Successfully
- Given I run `./create_pkg P2 spec -in dir9 -with P2` Successfully
- Given I run `./create_pkg P3 spec -in dir9 -with P2 -with P3` Successfully
- Given I run `./create_pkg P4 spec -in dir9 -with P2 -with P3` Successfully
- Given I run `./create_pkg Bus spec -in dir9` Successfully
- Given I run `./create_pkg IO spec -in dir9` Successfully
- Given the file `rules.9`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
```
- When I run `./acc -lr -I dir9 -ar "only P1 may use IO" rules.9`
- Then output is
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
P1 may use IO
```

> acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9

Expected

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
P1 may use IO
P2 may use Bus
P3 and P4 are independent
```

- Given there is no `dir9` directory
- Given I run `./create_pkg P1 spec -in dir9` Successfully
- Given I run `./create_pkg P2 spec -in dir9 -with P2` Successfully
- Given I run `./create_pkg P3 spec -in dir9 -with P2 -with P3` Successfully
- Given I run `./create_pkg P4 spec -in dir9 -with P2 -with P3` Successfully
- Given I run `./create_pkg Bus spec -in dir9` Successfully
- Given I run `./create_pkg IO spec -in dir9` Successfully
- Given the file `rules.9`
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
```
- When I run `./acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9`
- Then output is
```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
P1 may use IO
P2 may use Bus
P3 and P4 are independent
```
