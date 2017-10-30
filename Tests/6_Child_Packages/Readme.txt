Test Suite: Child packages tests

The tests hereafter are similar to those in <Layer rules tests>, except that child packages allows to simplify rules file.

A file rules :
(start code)
GUI contains P1 and P2
DB  contains P3 and P4
GUI is a layer over DB
(end)

is simplified in :
(start code)
GUI is a layer over DB
(end)
if Ada packages are named GUI.P1, GUI.P2, DB.P3 and DB.P4


Test 1:

Check that Layer rules apply to child packages
(see test_1.png)

This architecture is described by the following rules file :

(start code)

GUi is a layer over Db

(end)

The expected result is :
- no message
- normal return code


Test 2:

Check detection of a dependancy from a lower layer component to an upper layer component.
(see test_2.png)

This architecture is described by the following rules file :

(start code)

GUI is a layer over DB

(end)

The expected result is :
- the message : "Error : DB.P4 is in DB layer, and so shall not use the upper GUI layer"
- normal return code


Test 3:

Check detection of a dependancy link crossing a layer

(see test_3.png)

This architecture is described by the following rules file :

(start code)

GUI is a layer over DB

(end)

The expected result is :
- the message : "Error : P6 is not in GUI layer, and so shall not directly use DB layer"
- normal return code


Test 4:

Check detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer

(see test_4.png)

This architecture is described by the following rules file :

(start code)

GUI is a layer over DB

(end)

The expected result is :
- the message : "Warning : GUI.P2 (in GUI layer) uses P7 that is neither in the same layer, nor in the lower DB layer"
- normal return code
