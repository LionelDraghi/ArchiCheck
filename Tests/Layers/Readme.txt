File: Layer rules tests

Test 1:

Check that ArchiCheck doesn't complain on a normal situation.
(see test_1.png)

This architecture is described by the following rules file :

(start code)

Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B

(end)

The expected result is :
- no message
- normal return code


Test 2:

Check detection of a dependancy from a lower layer component to an upper layer component.
(see test_2.png)

This architecture is described by the following rules file :

(start code)

Layer_A contains P1, P2, P5
Layer_B contains P3, P4
Layer_A is a layer over Layer_B

(end)

The expected result is :
- the message : "Error : P4 is in Layer_B layer, and so shall not use the upper Layer_A layer"
- normal return code


Test 3:

Check detection of a dependancy link crossing a layer

(see test_3.png)

This architecture is described by the following rules file :

(start code)

Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B

(end)

The expected result is :
- the message : "Error : P6 is not in Layer_A layer, and so shall not directly use Layer_B layer"
- normal return code


Test 4:

Check detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer

(see test_4.png)

This architecture is described by the following rules file :

(start code)

Layer_A contains P1, P2
Layer_B contains P3, P4
Layer_A is a layer over Layer_B

(end)

The expected result is :
- the message : "Warning : P2 (in Layer_A layer) uses P7 that is neither in the same layer, nor in the lower Layer_B layer"
- normal return code
