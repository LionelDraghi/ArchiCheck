
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
 > archicheck rules1.txt -I dir1  

   With:        Expected:  
   X.P2 -> X.P1  OK  
   X.P2 -> Bus   OK  
   Y    -> Bus   OK  
   U    -> X.P2  OK  
   V    -> Y     OK  


Test Independent Components [Successful](tests_status.md#successful)

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
 > archicheck rules1.txt -I dir1  

   With:        Expected:  
   X.P2 -> X.P1  OK  
   X.P2 -> Bus   OK  
   Y    -> Bus   OK  
   U    -> X.P2  OK  
   V    -> Y     OK  
   Y.P3 -> X.P1  Error  


Test broken Independent Components rule [Successful](tests_status.md#successful)
