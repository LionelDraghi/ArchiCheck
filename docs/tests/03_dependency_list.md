# Feature: Dependencies identification simple test suite

## Scenario: Simple test

- Given the `./dir1/` dir

- Given the file `./dir1/a.ads`
```ada
with B;
package A is
end;
```    
- And   the file `./dir1/a.adb`    
```ada
with C;
package body A is
end;
```    
- And   the file `./dir1/b.adb`    
```ada
package body B is
end;
```    
- And   the file `./dir1/c.ads`    
```ada
with B;
with F;
package C is
end;
```    
- And   the file `./dir1/c-d.adb`    
```ada
with E.G.H ;
procedure C.D is
   null
end C.D;
```

- When I run `acc -I dir1 --list_dependencies`

- Then the output is (unordered) 
```  
A package body depends on C 
A package spec depends on B 
C.D procedure body depends on E.G.H 
C package spec depends on B 
C package spec depends on F 
```  

## Scenario: Source with weird formatted withed unit

  Same test here, but `c-d.adb` if slightly different

- Given the new file `./dir1/c-d.adb`    
```  
with   
     E, -- xyz
  F, G;
function C .D is
   null;
end C . D ;
```  

- When I run `./acc -I dir1 --list_dependencies`

- Then the output is (unordered)
```  
A package body depends on C 
A package spec depends on B 
C.D function body depends on E
C.D function body depends on F 
C.D function body depends on G 
C package spec depends on B 
C package spec depends on F 
```  
