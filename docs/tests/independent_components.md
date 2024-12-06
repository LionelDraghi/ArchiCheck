
##  Test Independent Components


   +---------+  +-----------+   
   |    X    |  |     Y     |   
   +---------+  +-----------+   
   +------------------------+   
   |          Bus           |   
   +------------------------+   

```  
X and Y are independent
```  

 Running :    
 > acc rules1.txt -I dir1  

   With:        Expected:  
   X.P2 -> X.P1  OK  
   X.P2 -> Bus   OK  
   Y    -> Bus   OK  
   U    -> X.P2  OK  
   V    -> Y     OK  


Test Independent Components [Successful](independent_components.md#--test-independent-components)

##  Test broken Independent Components rule


   +---------+  +-----------+   
   |    X    |  |     Y     |   
   +---------+  +-----------+   
   +------------------------+   
   |          Bus           |   
   +------------------------+   

```  
X and Y are independent
```  

 Running :    
 > acc rules1.txt -I dir1  

   With:        Expected:  
   X.P2 -> X.P1  OK  
   X.P2 -> Bus   OK  
   Y    -> Bus   OK  
   U    -> X.P2  OK  
   V    -> Y     OK  
   Y.P3 -> X.P1  Error  


Test broken Independent Components rule [Successful](independent_components.md#--test-broken-independent-components-rule)
