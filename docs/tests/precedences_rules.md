
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
Warning : src/lb-y.ads:3: LB.Y (in LB layer) uses Interfaces that is neither in the same layer, nor in the lower LC layer
Warning : src/lb-y.ads:4: LB.Y (in LB layer) uses Interfaces.Java that is neither in the same layer, nor in the lower LC layer
Error : src/lb-y.ads:6: LB.Y is in LB layer, and so shall not use LA.X in the upper LA layer
```  

Precedence rules unit test / Declaration of a component already existing in code [Successful](tests_status.md#successful)

##  Precedence rules unit test / Alowing a child of forbidden unit

  Let's forbid Interfaces and allow Interfaces.C  

```  
Interfaces   use is forbidden
Interfaces.C use is allowed

-- Fixme: and what if declared the other way round?
```  

  Running :    
  > archicheck rules2.txt -I src  

  Expected :  
```  
Error : src/lb-y.ads:3: Interfaces use is forbidden
Error : src/lb-y.ads:4: Interfaces.Java use is forbidden
```  

Precedence rules unit test / Alowing a child of forbidden unit [Successful](tests_status.md#successful)
