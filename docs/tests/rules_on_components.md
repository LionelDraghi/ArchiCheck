
# Component processing in rules unit test



##  Component processing in rules unit test / Layer component

  Here is the rules file :

```
LA is a Layer over LB
LB is a Layer over LC
```

  Running :  
  > archicheck rules1.txt -I src

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
  > archicheck rules2.txt -I src

  No more error/warning expected :
```
```

 Component processing in rules unit test / Layer component [Successful]("tests-status#successful")

##  Component processing in rules unit test / Env component allowed

  Let's add faulty units in an Env component, and allow Env use :

```
LA is a Layer over LB
LB is a Layer over LC

Env contains Interfaces.C and Ada
Env use is allowed

```

  Running :  
  > archicheck rules3.txt -I src

  No more error/warning expected :
```
```

 Component processing in rules unit test / Env component allowed [Successful]("tests-status#successful")
