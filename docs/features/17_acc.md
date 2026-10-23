# Feature: Acc code test suite <!-- omit from toc -->

- [Scenario: -lf test](#scenario--lf-test)
- [Scenario: -ld test](#scenario--ld-test)
- [Scenario: rules test](#scenario-rules-test)
- [Scenario: --list\_non\_covered](#scenario---list_non_covered)


## Scenario: -lf test

- Given there is no `src` directory
- And I run `unzip -q -o ../docs/features/17_acc/src.zip -d src` Successfully
- And there is a file `../docs/features/17_acc/expected_output.1`
- When I run `./acc -lf -I src` Successfully
- Then I get file `../docs/features/17_acc/expected_output.1` (ignoring order)

## Scenario: -ld test

- Given there is no `src` directory
- And I run `unzip -q -o ../docs/features/17_acc/src.zip -d src` Successfully
- And there is a file `../docs/features/17_acc/expected_output.2`
- When I run `./acc -ld -I ./src` Successfully
- Then I get file `../docs/features/17_acc/expected_output.2` (ignoring order)

## Scenario: rules test

- Given there is no `src` directory
- And I run `unzip -q -o ../docs/features/17_acc/src.zip -d src` Successfully
- And there is a file `../docs/features/17_acc/archicheck.ac`
- And there is a file `../docs/features/17_acc/expected_output.3`
- When I run `./acc ../docs/features/17_acc/archicheck.ac -I ./src` Successfully
- Then I get file `../docs/features/17_acc/expected_output.3`

## Scenario: --list_non_covered

- Given there is no `src` directory
- And I run `unzip -q -o ../docs/features/17_acc/src.zip -d src` Successfully
- And there is a file `../docs/features/17_acc/archicheck.ac`
- And there is a file `../docs/features/17_acc/expected_output.4`
- When I run `./acc ../docs/features/17_acc/archicheck.ac -lnc -I ./src` Successfully
- Then I get file `../docs/features/17_acc/expected_output.4`
