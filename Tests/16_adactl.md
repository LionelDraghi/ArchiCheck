## Feature: AdaControl code test suite

### Scenario: -lf test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf ../docs/tests/16_adactl/adactl-1.19r10-src.tgz` Successfully
- Given there is a file `../docs/tests/16_adactl/expected_output.1`
- When I run `./acc -lf -r -I adactl-1.19r10/src` Successfully
- Then I get file (unordered) `../docs/tests/16_adactl/expected_output.1`

### Scenario: -ld test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf ../docs/tests/16_adactl/adactl-1.19r10-src.tgz` Successfully
- Given there is a file `../docs/tests/16_adactl/expected_output.2`
- When I run `./acc -ld -r -I ./adactl-1.19r10/src` Successfully
- Then I get file (unordered) `../docs/tests/16_adactl/expected_output.2`

### Scenario: rules test

- Given there is no `adactl-1.19r10` directory
- Given I run `tar zxf ../docs/tests/16_adactl/adactl-1.19r10-src.tgz` Successfully
- Given there is a file `../docs/tests/16_adactl/adactl.ac`
- When I run `./acc ../docs/tests/16_adactl/adactl.ac -r -I ./adactl-1.19r10/src` Successfully
- Then I get file `../docs/tests/16_adactl/expected_output.3`
