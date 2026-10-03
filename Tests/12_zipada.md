## Feature: ZipAda code test suite

### Scenario: -lf test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully
- Given the file `12_ZipAda/expected_output.1`
- Given the file `12_ZipAda/zipadarules.txt`
- When I run `./acc -lf -r -I zip-ada` Successfully
- Then output matches file 12_ZipAda/expected_output.1

### Scenario: -ld test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully
- Given the file `12_ZipAda/expected_output.2`
- When I run `./acc -ld -r -I ./zip-ada | sort` Successfully
- Then output matches file 12_ZipAda/expected_output.2

### Scenario: rules test

- Given there is no `zip-ada` directory
- Given I run `unzip -q -o 12_ZipAda/zipada53.zip` Successfully
- Given the file `12_ZipAda/zipadarules.txt`
- When I run `./acc 12_ZipAda/zipadarules.txt -r -I ./zip-ada` Successfully
- Then output is empty
