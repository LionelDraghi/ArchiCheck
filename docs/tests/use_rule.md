
# Use rules test suite



##  Use rules test suite / May_Use rule, code compliant, no output expected

  Code compliant with the rules, should be OK  

```  
Component_A contains P1, P2
Component_B contains P3, P4

only Component_B may use Interfaces
```  


Use rules test suite / May_Use rule, code compliant, no output expected [Successful](use_rule.md#use-rules-test-suite--mayuse-rule-code-compliant-no-output-expected)

##  Use rules test suite / Using a unit from a non allowed unit

  P4 body is using Interfaces.C : OK    
  P1 body is using Interfaces.C : should complain  

  Expecting :  

```  
Error : dir2/p1.adb:1: Only Component_B is allowed to use Interfaces, P1 is not
```  


Use rules test suite / Using a unit from a non allowed unit [Successful](use_rule.md#use-rules-test-suite--using-a-unit-from-a-non-allowed-unit)

##  Use rules test suite / Forbidden use test


```  
P4 use is forbidden
```  

  Expecting :  

```  
Error : dir3/p2.adb:1: P4 use is forbidden
```  


Use rules test suite / Forbidden use test [Successful](use_rule.md#use-rules-test-suite--forbidden-use-test)

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


Use rules test suite / Allowing use of an environnement package [Successful](use_rule.md#use-rules-test-suite--allowing-use-of-an-environnement-package)

##  Use rules test suite / Cumulative only ... may use X rules

  P1 P2 and P3 are withing Interfaces.C  

  rules5.txt contains :  

```  
only P1 may use Interfaces.C
only P2 may use Interfaces.C

P4 may use Interfaces.C

-- Note that the same error message will be output twice here, because checks
-- are done once per rule, and there is two lines "only ... may use P3".
-- This situation (that is to have more than one "only" statement targeting
-- the same unit) is not coherent, but I won't change the code and try to fix that.
-- It will remain a "feature".
--
-- The future syntax "only P1 and P2 may use Interfaces.C" could be a solution.
--
-- 29/12/2017, Lionel
```  

  When running :  

  > acc -I dir5 rules.5  

  Expected :  

```  
Error : dir5/p3.ads:1: Only P1, P2 and P4 are allowed to use Interfaces.C, P3 is not
Error : dir5/p3.ads:1: Only P1, P2 and P4 are allowed to use Interfaces.C, P3 is not
```  


Use rules test suite / Cumulative only ... may use X rules [Successful](use_rule.md#use-rules-test-suite--cumulative-only--may-use-x-rules)

##  Use rules test suite / Combining Allowed and Forbidden

  P1 P2 and P3 are withing Interfaces, Interfaces.C and Interfaces.Java  

  rules6.txt contains :  

```  
Interfaces   use is forbidden
Interfaces.C use is allowed
```  

  When running :  

  > acc -I dir6 rules.6  

  Expected :  
  And now inverting Allowed and Forbidden  

  rules6b.txt contains :  

```  
Interfaces   use is allowed
Interfaces.C use is forbidden

```  

  When running :  

  > acc -I dir6 rules.6b  

  Expected :  

Use rules test suite / Combining Allowed and Forbidden [Successful](use_rule.md#use-rules-test-suite--combining-allowed-and-forbidden)

##  Use rules test suite / X may use Unit List rules

  P1 withing P2, P3, P4  

  rules7.txt contains :  

```  
P1 may use P2 and P3
```  

  When running :  

  > acc -I dir7 rules.7  

  Expected  

```  
Error : dir7/p3.ads:1: P1 may use P3, so P3 shall not use P1
Error : dir7/p2.ads:1: P1 may use P2, so P2 shall not use P1
```  


Use rules test suite / X may use Unit List rules [Successful](use_rule.md#use-rules-test-suite--x-may-use-unit-list-rules)

##  Use rules test suite / only X may use Unit List rules

  P1 withing P2 and P3  
  P4 withing P2 and P3  

  rules8.txt contains :  

```  
only P1 may use P3
```  

  When running :  

  > acc -I dir8 rules.8  

  Expected  

```  
Error : dir8/p4.ads:2: Only P1 is allowed to use P3, P4 is not
```  


Use rules test suite / only X may use Unit List rules [Successful](use_rule.md#use-rules-test-suite--only-x-may-use-unit-list-rules)

##  Use rules test suite / Appending rules


  rules.9 contains :  

```  
Layer_A contains P1 and P2
Layer_B contains P3 and P4

```  

  When running :  

  > acc -lr -I dir9 -ar "only P1 may use IO" rules.9  

  Expected  

```  
rules.9:2: Component Layer_A contains unit P1 and P2
rules.9:2: Component Layer_B contains unit P3 and P4
Cmd line: Only P1 may use IO
```  

  > acc -lr -I dir9 -ar "P2 may use Bus" --append_rule "P3 and P4 are independent" rules.9  

  Expected  

```  
rules.9:2: Component Layer_A contains unit P1 and P2
rules.9:2: Component Layer_B contains unit P3 and P4
Cmd line: P2 may use Bus
Cmd line: P3 and P4 are independent```  


Use rules test suite / Appending rules [Successful](use_rule.md#use-rules-test-suite--appending-rules)
