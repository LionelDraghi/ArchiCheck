
# Dependencies identification simple test suite



##  Dependencies identification simple test suite / Simple test

  (src not detailed here)  
  > acc -I dir1 --list_dependencies  
  should put:  

```  
A package body depends on C 
A package spec depends on B 
C.D procedure body depends on E.G.H 
C package spec depends on B 
C package spec depends on F 
```  


Dependencies identification simple test suite / Simple test [Successful](dependency_list.md#dependencies-identification-simple-test-suite--simple-test)

##  Dependencies identification simple test suite / Source with weird formatted withed unit

  same test here, but `c-d.adb` if formated this way :  

```  
with   
     E, -- xyz
  F, G;
function C .D is
   null;
end C . D ;
```  


Dependencies identification simple test suite / Source with weird formatted withed unit [Successful](dependency_list.md#dependencies-identification-simple-test-suite--source-with-weird-formatted-withed-unit)
