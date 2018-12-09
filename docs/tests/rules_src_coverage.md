
# Rules vs sources coverage test suite



##  Rules vs sources coverage test suite / Warnings on units appearing in rules file and not related to any source


  Sources (p1.ads
p2.ads
p5.ads) :  

```  
p1.ads
p2.ads
p5.ads
```  

  Rules file :  

```  
Framework contains P1, P2 and P3
-- no P3 src, should raise a warning

P4 may use P1 
-- no P4 src, should raise a warning

Java.IO use is forbidden
-- no Java.IO sources, but should not raise a warning

P5 may use Framework
-- Framework match no sources, but should not raise warning
-- as it's a Component.

```  

  When :  

  > archicheck test1.ac -I ./dir1  

  Expected :  

```  
Warning : test1.ac:4: P3 do not match any compilation unit
Warning : test1.ac:7: P4 do not match any compilation unit
```  


Rules vs sources coverage test suite / Warnings on units appearing in rules file and not related to any source [Successful](tests_status.md#successful)

##  Rules vs sources coverage test suite / Non covered sources

  Rules file :  

```  
Framework contains P1, P2 and P3
-- no P3 src, should raise a warning

P4 may use P1 
-- no P4 src, should raise a warning

Java.IO use is forbidden
-- no Java.IO sources, but should not raise a warning

P5 may use Framework
-- Framework match no sources, but should not raise warning
-- as it's a Component.

```  

  Sources (framework_utilities.ads
framework-utilities.ads
java.ads
java-awt.ads
p1-x.ads
p2.ads
p3.ads
p4.ads
p5.ads
y-p1.ads) :  

```  
framework_utilities.ads
framework-utilities.ads
java.ads
java-awt.ads
p1-x.ads
p2.ads
p3.ads
p4.ads
p5.ads
y-p1.ads
```  

  When :  

  > archicheck -lnc test2.ac -I ./dir2  

  Expected :  

```  
Framework_Utilities
Java
Java.Awt
Y.P1
```  


Rules vs sources coverage test suite / Non covered sources [Successful](tests_status.md#successful)

##  Rules vs sources coverage test suite / Case insensitivity of Is_A_Component function (non reg)

  Rules file :  

```  
-- Check correction of a bug due to case sensitivity of Unit_Name default "="

SYSTEM contains org.springframework.samples.petclinic.system.WelcomeController

Org.SpringFramework may use Model, system
-- Model is not a component => Warning because of no Matching sources
-- system is a component    => no warning expected system should be identified as
--                             the component declared in uppercase.
```  

  When :  

  > archicheck test3.ac -I dir3  

  Expected :  

```  
Warning : test3.ac:8: Org.SpringFramework do not match any compilation unit
Warning : test3.ac:5: org.springframework.samples.petclinic.system.WelcomeController do not match any compilation unit
Warning : test3.ac:8: Model do not match any compilation unit
```  


Rules vs sources coverage test suite / Case insensitivity of Is_A_Component function (non reg) [Successful](tests_status.md#successful)
