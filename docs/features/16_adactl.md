# Feature: AdaControl code test suite <!-- omit from toc -->

- [Scenario: -lf test](#scenario--lf-test)
- [Scenario: -ld test](#scenario--ld-test)
- [Scenario: rules test](#scenario-rules-test)


## Scenario: -lf test

- Given there is no `adactl-1.19r10` directory
- And I run `tar zxf ../docs/features/16_adactl/adactl-1.19r10-src.tgz` Successfully
- And there is a file `../docs/features/16_adactl/expected_output.1`
- When I run `./acc -lf -r -I adactl-1.19r10/src` Successfully
- Then I get file `../docs/features/16_adactl/expected_output.1` (ignoring order)

## Scenario: -ld test

- Given there is no `adactl-1.19r10` directory
- And I run `tar zxf ../docs/features/16_adactl/adactl-1.19r10-src.tgz` Successfully
- And there is a file `../docs/features/16_adactl/expected_output.2`
- When I run `./acc -ld -r -I ./adactl-1.19r10/src` Successfully
- Then I get file `../docs/features/16_adactl/expected_output.2` (ignoring order)

## Scenario: rules test

- Given there is no `adactl-1.19r10` directory
- And I run `tar zxf ../docs/features/16_adactl/adactl-1.19r10-src.tgz` Successfully
- And there is a file `../docs/features/16_adactl/adactl.ac`
- When I run `./acc ../docs/features/16_adactl/adactl.ac -r -I ./adactl-1.19r10/src` Successfully
- Then I get file `../docs/features/16_adactl/expected_output.3`
