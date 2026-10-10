## Feature: Acc code test suite

### Scenario: -lf test

- Given there is no `src` directory
- Given I run `unzip -q -o ../docs/tests/17_acc/src.zip -d src` Successfully
- Given there is a file `../docs/tests/17_acc/expected_output.1`
- When I run `./acc -lf -I src` Successfully
- Then I get file (unordered) `../docs/tests/17_acc/expected_output.1`

### Scenario: -ld test

- Given there is no `src` directory
- Given I run `unzip -q -o ../docs/tests/17_acc/src.zip -d src` Successfully
- Given there is a file `../docs/tests/17_acc/expected_output.2`
- When I run `./acc -ld -I ./src` Successfully
- Then I get file (unordered) `../docs/tests/17_acc/expected_output.2`

### Scenario: rules test

- Given there is no `src` directory
- Given I run `unzip -q -o ../docs/tests/17_acc/src.zip -d src` Successfully
- Given there is a file `../docs/tests/17_acc/archicheck.ac`
- Given there is a file `../docs/tests/17_acc/expected_output.3`
- When I run `./acc ../docs/tests/17_acc/archicheck.ac -I ./src` Successfully
- Then I get file `../docs/tests/17_acc/expected_output.3`

### Scenario: --list_non_covered

- Given there is no `src` directory
- Given I run `unzip -q -o ../docs/tests/17_acc/src.zip -d src` Successfully
- Given there is a file `../docs/tests/17_acc/archicheck.ac`
- Given there is a file `../docs/tests/17_acc/expected_output.4`
- When I run `./acc ../docs/tests/17_acc/archicheck.ac -lnc -I ./src` Successfully
- Then I get file `../docs/tests/17_acc/expected_output.4`
