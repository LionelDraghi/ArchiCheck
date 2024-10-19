
# Independent Component test suite



##  Independent Component test suite / Test Independent Components


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


Independent Component test suite / Test Independent Components [Successful](independent_components.md#independent-component-test-suite--test-independent-components)

##  Independent Component test suite / Test broken Independent Components rule


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


Independent Component test suite / Test broken Independent Components rule [Successful](independent_components.md#independent-component-test-suite--test-broken-independent-components-rule)
