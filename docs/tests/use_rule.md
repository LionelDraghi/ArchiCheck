
# Use rules test suite



##  Use rules test suite / May_Use rule, code compliant, no output expected

  Code compliant with the rules, should be OK

```
Component_A contains P1, P2
Component_B contains P3, P4

only Component_B may use Interfaces



```


 Use rules test suite / May_Use rule, code compliant, no output expected [Successful]("tests-status#successful")

##  Use rules test suite / Using a unit from a non allowed unit

  P4 body is using Interfaces.C : OK
  P1 body is using Interfaces.C : should complain

```
Error : Only Component_B is allowed to use Interfaces, P1 is not
```


 Use rules test suite / Using a unit from a non allowed unit [Successful]("tests-status#successful")

##  Use rules test suite / Forbidden use test


```
P4 use is forbidden
```

```
Error : in file /home/lionel/Proj/Archicheck/Tests/10_Use_Rule/dir3/p2.adb : P4 use is forbidden
```


 Use rules test suite / Forbidden use test [Successful]("tests-status#successful")

##  Use rules test suite / Allowing use of an environnement package


```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
```

```
Warning : P2 (in Layer_A layer) uses Containers.Generic_Sort that is neither in the same layer, nor in the lower Layer_B layer
```

  First test without the allowing rule : should complain
  And now with the allowing rule :

```
Layer_A contains P1 and P2
Layer_B contains P3 and P4
Layer_A is a layer over Layer_B
Containers use is allowed
```


 Use rules test suite / Allowing use of an environnement package [Successful]("tests-status#successful")
