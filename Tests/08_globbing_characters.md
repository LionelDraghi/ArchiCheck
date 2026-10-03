## Feature: Globbing Character test suite

### Scenario: rules test

  Testing this dependencies :  

  ![](gc1.png)  

  against this rules file :

- Given the file `rules.1`
```  
Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces*
```  

- When I run `./create_pkg P1 spec -in dir1 -with P2`
- When I run `./create_pkg P2 spec  -in dir1 -with P3 -with P4` 
- When I run `./create_pkg P3 spec  -in dir1 -with Interfaces.C`
- When I run `./create_pkg P4 spec  -in dir1 -with Interfaces.C.Strings`
  
- When I run `./acc -I dir1 rules.1` 
   
- Then there is no error
- And  there is no output

### Scenario: illegal use of Interfaces from Application_Layer

  Testing this dependencies :  

  ![](gc2.png)  

  against this rules file :  

- Given the file `rules.2`
```  
Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces*
```  

- When I run `./create_pkg P1 spec -in dir2 -with P2 -with Interfaces.C`
- When I run `./create_pkg P2 spec -in dir2 -with P3 -with P4` 
- When I run `./create_pkg P3 spec -in dir2 -with Interfaces.C`
- When I run `./create_pkg P4 spec -in dir2 -with Interfaces.C.Strings`

- When I run `./acc -I dir2 rules.2` 
   
- Then I get
```  
Error : dir2/p1.ads:2: Only Support_Layer is allowed to use Interfaces*, P1 is not
```  
