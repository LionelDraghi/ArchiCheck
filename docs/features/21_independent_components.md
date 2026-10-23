# Feature: Independent Component test suite <!-- omit from toc -->

- [Scenario: Test Independent Components](#scenario-test-independent-components)
- [Scenario: Test broken Independent Components rule](#scenario-test-broken-independent-components-rule)


This test suite validates the independent components functionality.

## Scenario: Test Independent Components

```mermaid
%%{init: {'theme':'base'}}%%
flowchart TB

  subgraph Layer_A [" "]
    direction LR
    subgraph X["X"]
      P1["P1"]
      P2["P2"]
    end
    Y["Y"]
    U["U"]
    V["V"]
  end
  style Layer_A fill:none,stroke:none

  Bus["Bus"]

  P2 -- with --> P1
  P2 -- with --> Bus
  Y -- with --> Bus
  U -- with --> P2
  V -- with --> Y
```

All dependencies stay inside the X or Y component families:
X and Y are independent, no output expected.

- Given there is no `dir1` directory
- And   I run `./create_pkg X.P1 spec -in dir1` Successfully
- And   I run `./create_pkg Bus spec -in dir1` Successfully
- And   I run `./create_pkg X.P2 spec -in dir1 -with X.P1 -with Bus` Successfully
- And   I run `./create_pkg Y spec -in dir1 -with Bus` Successfully
- And   I run `./create_pkg U spec -in dir1 -with X.P2` Successfully
- And   I run `./create_pkg V spec -in dir1 -with Y` Successfully
- Given the file `rules1.txt`
```
X and Y are independent
```
- When I run `./acc rules1.txt -I dir1`
- Then I get no output

## Scenario: Test broken Independent Components rule

```mermaid
%%{init: {'theme':'base'}}%%
flowchart TB

  subgraph Layer_A [" "]
    direction LR
    subgraph Y["Y"]
      P3["P3"]
    end
    U["U"]
    V["V"]
    subgraph X["X"]
      P1["P1"]
      P2["P2"]
    end
  end
  style Layer_A fill:none,stroke:none

  Bus["Bus"]

  P2 -- with --> P1
  P2 -- with --> Bus
  Y -- with --> Bus
  U -- with --> P2
  V -- with --> Y
  P3 -- "illegal with" --> P1
  linkStyle 5 stroke:red
```

- Given there is no `dir1` directory
- And   I run `./create_pkg X.P1 spec -in dir1` Successfully
- And   I run `./create_pkg Bus spec -in dir1` Successfully
- And   I run `./create_pkg X.P2 spec -in dir1 -with X.P1 -with Bus` Successfully
- And   I run `./create_pkg Y spec -in dir1 -with Bus` Successfully
- And   I run `./create_pkg Y.P3 spec -in dir1 -with X.P1` Successfully

- Given the file `rules1.txt`
```
X and Y are independent
```
- When I run `./acc rules1.txt -I dir1`
- Then output is
```
Error : dir1/y-p3.ads:1: X and Y must be independent
```
