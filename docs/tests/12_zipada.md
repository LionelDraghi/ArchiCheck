## Feature: ZipAda code test suite

### Scenario: -lf test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o ../docs/tests/12_zipada/zipada53.zip` Successfully
- Given there is a file `../docs/tests/12_zipada/expected_output.1`
- Given there is a file `../docs/tests/12_zipada/zipadarules.txt`
- When I successfully run `./acc -lf -r -I zip-ada` or `./acc -lf --recursive -I zip-ada`
- Then I get file `../docs/tests/12_zipada/expected_output.1`

### Scenario: -ld test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o ../docs/tests/12_zipada/zipada53.zip` Successfully
- Given there is a file `../docs/tests/12_zipada/expected_output.2`
- When I run `./acc -ld -r -I ./zip-ada` Successfully
- Then I get file (unordered) `../docs/tests/12_zipada/expected_output.2`

### Scenario: rules test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o ../docs/tests/12_zipada/zipada53.zip` Successfully
- Given there is a file `../docs/tests/12_zipada/zipadarules.txt`
- When I run `./acc ../docs/tests/12_zipada/zipadarules.txt -r -I ./zip-ada` Successfully
- Then I get no output
