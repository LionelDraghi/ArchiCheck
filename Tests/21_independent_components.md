## Feature: Independent Component test suite

This test suite validates the independent components functionality.

### Scenario: Test Independent Components

- Given there is no `dir1` directory
- Given I run `./create_pkg X.P1 spec -in dir1` Successfully
- Given I run `./create_pkg Bus spec -in dir1` Successfully
- Given the file `dir1/x-p2.ads`
```ada
with X.P1, Bus;
package X.P2 is
end X.P2;
```
- Given the file `dir1/y.ads`
```ada
with Bus;
package Y is
end Y;
```
- Given the file `dir1/u.ads`
```ada
with X.P2;
package U is
end U;
```
- Given the file `dir1/v.ads`
```ada
with Y;
package V is
end V;
```
- Given the file `rules1.txt`
```
X and Y are independent
```
- When I run `./acc rules1.txt -I dir1`
- Then output is empty

### Scenario: Test broken Independent Components rule

- Given there is no `dir1` directory
- Given I run `./create_pkg X.P1 spec -in dir1` Successfully
- Given I run `./create_pkg Bus spec -in dir1` Successfully
- Given the file `dir1/x-p2.ads`
```ada
with X.P1, Bus;
package X.P2 is
end X.P2;
```
- Given the file `dir1/y.ads`
```ada
with Bus;
package Y is
end Y;
```
- Given the file `dir1/y-p3.ads`
```ada
with X.P1;
package Y.P3 is
end Y.P3;
```
- Given the file `dir1/u.ads`
```ada
with X.P2;
package U is
end U;
```
- Given the file `dir1/v.ads`
```ada
with Y;
package V is
end V;
```
- Given the file `rules1.txt`
```
X and Y are independent
```
- When I run `./acc rules1.txt -I dir1`
- Then output is
```
Error : dir1/y-p3.ads:1: X and Y must be independent
```
