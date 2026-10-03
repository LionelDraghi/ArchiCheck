## Feature: AdaControl code test suite

### Scenario: -lf test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully
- Given the file `16_AdaControl/expected_output.1`
- When I run `./acc -lf -r -I adactl-1.19r10/src` Successfully
- Then output matches file (unordered) 16_AdaControl/expected_output.1

### Scenario: -ld test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully
- Given the file `16_AdaControl/expected_output.2`
- When I run `./acc -ld -r -I ./adactl-1.19r10/src` Successfully
- Then output matches file (unordered) 16_AdaControl/expected_output.2

### Scenario: rules test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf 16_AdaControl/adactl-1.19r10-src.tgz` Successfully
- Given the file `16_AdaControl/adactl.ac`
- When I run `./acc 16_AdaControl/adactl.ac -r -I ./adactl-1.19r10/src` Successfully
- Then output matches file 16_AdaControl/expected_output.3
