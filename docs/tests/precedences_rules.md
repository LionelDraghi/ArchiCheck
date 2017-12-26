
# Precedence rules unit test



##  Precedence rules unit test / Declaration of a component already existing in code

  Here is the rules file :

```
LA is a Layer over LB
LB is a Layer over LC

LC contains Interfaces.C and Ada

```

  But there is already a package named LC.

  When running :  
  > archicheck rules1.txt -I src

  Expected :
```
Warning : LB.Y (in LB layer) uses LC.Z that is neither in the same layer, nor in the lower LC layer
Error : LB.Y is in LB layer, and so shall not use LA.X in the upper LA layer
Warning : LB.Y (in LB layer) uses LA.X that is neither in the same layer, nor in the lower LC layer
```

** Precedence rules unit test / Declaration of a component already existing in code [Failed](tests_status.md#failed)**

##  Precedence rules unit test / Alowing a child of forbidden unit

  Let's forbid Interfaces and allow Interfaces.C

```
Interfaces   use is forbidden
Interfaces.C use is allowed

-- Fixme: and what if declared the other way round?
```

  Running :  
  > archicheck rules2.txt -I src

  No more error/warning expected :
```
Error : in file /home/lionel/Proj/Archicheck/Tests/15_Precedences_Rules/src/lb-y.ads : Interfaces use is forbidden
Error : in file /home/lionel/Proj/Archicheck/Tests/15_Precedences_Rules/src/lb-y.ads : Interfaces.Java use is forbidden
```

** Precedence rules unit test / Alowing a child of forbidden unit [Failed](tests_status.md#failed)**
