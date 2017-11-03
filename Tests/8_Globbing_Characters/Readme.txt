Test Suite: Globbing Characters tests

Test 1:

Check that ArchiCheck doesn't complain on a normal situation.
(see test_1.png)

This architecture is described by the following rules file :

(start code)

Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces.*

(end)

The expected result is :
- no message
- normal return code


Test 2:

Check illegal use of Interfaces from Application_Layer.
(see test_2.png)

This architecture is described by the following rules file :

(start code)

Application_Layer contains P1, P2
Support_Layer contains P3, P4
only Support_Layer may use Interfaces.*

(end)

The expected result is :
- the message : "Error : P4 is not in Support_Layer layer, and so shall not use Interfaces.* packages"
- normal return code


