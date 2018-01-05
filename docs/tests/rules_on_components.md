
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
Warning : src/lb-y.ads:1: LB.Y (in LB layer) uses Ada.Containers that is neither in the same layer, nor in the lower LC layer
Warning : src/lb-y.ads:2: LB.Y (in LB layer) uses Interfaces.C that is neither in the same layer, nor in the lower LC layer
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

  Running :  
  > archicheck rules3.txt -I src

  No more error/warning expected :

  > Note that line numbers are false in error messages due to
  > a strange OpenToken bug (I guess).

```
Error : rules3.txt:25: P1 already in X (cf. rules3.txt:3: ), can't be added to Y
Error : rules3.txt:26: P1 already in X (cf. rules3.txt:3: ), can't be added to Z
```

Component processing in rules unit test / Trying to include a unit in more components [Successful](tests_status.md#successful)

##  Component processing in rules unit test / Test on Components embedding components embedding components...


```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden
```

  Running :  
  > archicheck rules4.txt -I dir4

  Error expected, as P1 is using a forbidden unit :
```
Error : dir4/p1.ads:2: Interfaces.C use is forbidden
```
  Lets allow Interfaces.C use through Z components :

```
X contains P1
Y contains X
Z contains Y

Interfaces.C use is forbidden

Z may use Interfaces.C
```

  Running :  
  > archicheck rules4.txt -I dir4

  No more Error expected.

Component processing in rules unit test / Test on Components embedding components embedding components... [Successful](tests_status.md#successful)

##  Component processing in rules unit test / Test A B C exemple posted on fr.comp.lang.ada...

    Except that as "A" is a reserved word, I use X Y Z instead :-)

    +----------++------------+ 
    |     X    ||     Y      | 
    +----------++------------+ 
    +------------------------+ 
    |           Z            | 
    +------------------------+ 

```
My_Layer contains X and Y
    
My_Layer is a layer over Z
```

  Running :  
  > archicheck rules5.txt -I dir5

    With:        Expected:
    Y.P1 -> X.P1  OK
    Y.P2 -> Z.P1  OK
    Z.P1 -> X     Error
    Z.P2 -> Y.P2  Error
    U    -> Z.P2  Warning
    V    -> Y.P1  OK


Component processing in rules unit test / Test A B C exemple posted on fr.comp.lang.ada... [Successful](tests_status.md#successful)
