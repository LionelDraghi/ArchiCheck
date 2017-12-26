
# Component processing in rules unit test



##  Component processing in rules unit test / Layer component

  Here is the rules file :

```
LA is a Layer over LB
LB is a Layer over LC
```

  Running :  
  > archicheck rules1a.txt -I src

  Expected :
```
Warning : LB.Y (in LB layer) uses Ada.Containers that is neither in the same layer, nor in the lower LC layer
Warning : LB.Y (in LB layer) uses Interfaces.C that is neither in the same layer, nor in the lower LC layer
```
  Let's now add faulty units in LC layer :

```
LA is a Layer over LB
LB is a Layer over LC

LC contains Interfaces.C and Ada

```

  Running :  
  > archicheck rules1b.txt -I src

  No more error/warning expected :
```
```

 Component processing in rules unit test / Layer component [Successful](tests_status.md#successful)

##  Component processing in rules unit test / Env component allowed

  Let's add faulty units in an Env component, and allow Env use :

```
LA is a Layer over LB
LB is a Layer over LC

Env contains Interfaces.C and Ada
Env use is allowed

```

  Running :  
  > archicheck rules2.txt -I src

  No more error/warning expected :
```
```

 Component processing in rules unit test / Env component allowed [Successful](tests_status.md#successful)

##  Component processing in rules unit test / Trying to include a unit in more components

  P1 package is in X component, wha if I try to add it to another components?

```
X contains P1 and P2 -- P1 to P4 are existing compilation units
Y contains P3 and P4

Z contains X and Y -- Component of components

-- till now, OK.

-- and now let's try to add P1 (that belong to X) to another components :
Y contains P1
Z contains P1
```

  Running :  
  > archicheck rules3.txt -I src

  No more error/warning expected :
```
Error: P1 appartient au composant X, ne peut pas être ajouté au composant Y
Warning : P1 appartient déjà au composant Z par l'intermédiaire de X, ligne ignorée```

** Component processing in rules unit test / Trying to include a unit in more components [Failed](tests_status.md#failed)**

##  Component processing in rules unit test / Test on Components embedding components embedding components...


```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden
```

  Running :  
  > archicheck rules4.txt -I dir4

  Error expected, as P1 is unsing a forbidden unit :
```
Error : in file /home/lionel/Proj/Archicheck/Tests/14_Rules_On_Components/dir4/p1.ads : Interfaces.C use is forbidden
```
  Lets allow Interfaces.C use through Z components :

```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden

Z use Interfaces.C
```

  Running :  
  > archicheck rules4.txt -I dir4

  No more Error expected.

** Component processing in rules unit test / Test on Components embedding components embedding components... [Failed](tests_status.md#failed)**
