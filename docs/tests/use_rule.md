
# Use rules test suite



##  Use rules test suite / May_Use rule, code compliant, no output expected

  Code compliant with the rules, should be OK

```
Component_A contains P1, P2
Component_B contains P3, P4

only Component_B may use Interfaces
```


Use rules test suite / May_Use rule, code compliant, no output expected [Successful](tests_status.md#successful)

##  Use rules test suite / Using a unit from a non allowed unit

  P4 body is using Interfaces.C : OK
  P1 body is using Interfaces.C : should complain

  Expecting :

```
Error : dir2/p1.adb:1: Only Component_B is allowed to use Interfaces, P1 is not
```


Use rules test suite / Using a unit from a non allowed unit [Successful](tests_status.md#successful)

##  Use rules test suite / Forbidden use test


```
P4 use is forbidden
```

  Expecting :

```
Error : dir3/p2.adb:1: P4 use is forbidden
```


Use rules test suite / Forbidden use test [Successful](tests_status.md#successful)

##  Use rules test suite / Allowing use of an environnement package


  Rules :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
```
  Expecting :


```
Warning : dir4/p2.ads:1: P2 (in Layer_A layer) uses Containers.Generic_Sort that is neither in the same layer, nor in the lower Layer_B layer
```

  First test without the allowing rule : should complain
  And now with the allowing rule :

  Rules :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
Containers use is allowed
```

  No error expected.


Use rules test suite / Allowing use of an environnement package [Successful](tests_status.md#successful)

##  Use rules test suite / Cumulative only ... may use X rules

  P1 P2 and P3 are withing Interfaces.C

  rules5.txt contains :

```
only P1 may use Interfaces.C
only P2 may use Interfaces.C

P4 may use Interfaces.C

-- Note that the same error message will be output twice here, because checks
-- are done once per rule, and there are two lines "only ... may use P3".
-- As this situation (that is to have more than one "only" statement targetting
-- the same unit) is not coherent, I won't change the code and try to fix that.
-- It will remain a "feature".
-- The future syntax "only P1 and P2 may use Interfaces.C" could be a solution.
--
-- 29/12/2017, Lionel
```

  When running :

  > archicheck -I dir5 rules.5

  Expected :

```
Error : dir5/p3.ads:1: Only P1, P2 and P4 are allowed to use Interfaces.C, P3 is not
Error : dir5/p3.ads:1: Only P1, P2 and P4 are allowed to use Interfaces.C, P3 is not
```


Use rules test suite / Cumulative only ... may use X rules [Successful](tests_status.md#successful)

##  Use rules test suite / Combining Allowed and Forbidden

  P1 P2 and P3 are withing Interfaces, Interfaces.C and Interfaces.Java

  rules6.txt contains :

```
Interfaces   use is forbidden
Interfaces.C use is allowed
```

  When running :

  > archicheck -I dir6 rules.6

  Expected :
  And now inverting Allowed and Forbidden

  rules6b.txt contains :

```
Interfaces   use is allowed
Interfaces.C use is forbidden

```

  When running :

  > archicheck -I dir6 rules.6b

  Expected :

Use rules test suite / Combining Allowed and Forbidden [Successful](tests_status.md#successful)
