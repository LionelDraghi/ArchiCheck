
# Globbing Character test suite



##  Globbing Character test suite / rules test


  Testing this dependencies :  

  ![](gc1.png)  

  against this rules file :  

```  
Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces*
```  

  No expected output (no error)  

Globbing Character test suite / rules test [Successful](globbing_characters.md#globbing-character-test-suite--rules-test)

##  Globbing Character test suite / illegal use of Interfaces from Application_Layer


  Testing this dependencies :  

  ![](gc2.png)  

  against this rules file :  

```  
Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces*
```  

  Expected output :  

```  
Error : dir2/p1.ads:2: Only Support_Layer is allowed to use Interfaces*, P1 is not
```  

Globbing Character test suite / illegal use of Interfaces from Application_Layer [Successful](globbing_characters.md#globbing-character-test-suite--illegal-use-of-interfaces-from-applicationlayer)
