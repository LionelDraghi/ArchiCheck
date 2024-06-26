
# Child packages test suite


 The tests hereafter are similar to those in [Layer rules tests suite], except that child packages allows to simplify rules file.  

 The file rules :  

```  
 GUI contains P1 and P2  
 DB  contains P3 and P4  
 GUI is a layer over DB  
```  

 is simplified in :  

```  
GUI is a layer over DB
```  

 and packages GUI.P1, GUI.P2, DB.P3 and DB.P4 are created.  

##  Child packages test suite / Rules OK test, no output expected

  ![](cp1.png)  

```  
GUI is a layer over DB
```  


Child packages test suite / Rules OK test, no output expected [Successful](child_packages.md#child-packages-test-suite--rules-ok-test-no-output-expected)

##  Child packages test suite / Reverse dependency test

  Detection of a dependancy from a lower layer component to an upper layer component.  
  ![](cp2.png)  

  Expected output :  

```  
Error : dir2/db-p4.adb:1: DB.P4 is in DB layer, and so shall not use GUI.P5 in the upper GUI layer
```  


Child packages test suite / Reverse dependency test [Successful](child_packages.md#child-packages-test-suite--reverse-dependency-test)

##  Child packages test suite / Layer bridging test

  Detection of a dependancy link crossing a layer.  
  ![](cp3.png)  

  Expected output :  

```  
Warning : dir3/p6.ads:1: P6 is neither in GUI or DB layer, and so shall not directly use DB.P4 in the DB layer
```  


Child packages test suite / Layer bridging test [Successful](child_packages.md#child-packages-test-suite--layer-bridging-test)

##  Child packages test suite / Undescribed dependency test

  Detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer.  
  ![](cp4.png)  

  Expected output :  

```  
Warning : dir4/gui-p2.ads:3: GUI.P2 (in GUI layer) uses P7 that is neither in the same layer, nor in the lower DB layer
```  


Child packages test suite / Undescribed dependency test [Successful](child_packages.md#child-packages-test-suite--undescribed-dependency-test)

##  Child packages test suite / Packages in the same layer may with themself


Child packages test suite / Packages in the same layer may with themself [Successful](child_packages.md#child-packages-test-suite--packages-in-the-same-layer-may-with-themself)

##  Child packages test suite / GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg


Child packages test suite / GUI.P1 is a GUI child, GUIP1 is not a GUI child pkg [Successful](child_packages.md#child-packages-test-suite--guip-is-a-gui-child-guip-is-not-a-gui-child-pkg)

##  Child packages test suite / Forbidding Interfaces but allowing Interfaces.C


Child packages test suite / Forbidding Interfaces but allowing Interfaces.C [Successful](child_packages.md#child-packages-test-suite--forbidding-interfaces-but-allowing-interfacesc)
