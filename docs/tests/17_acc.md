## Feature: Acc code test suite

### Scenario: -lf test

- Given there is no `src` directory
- Given I run `unzip -q -o 17_Acc/src.zip` Successfully
- Given the file `17_Acc/expected_output.1`
- When I run `./acc -lf -I src | sort` Successfully
- Then output matches file 17_Acc/expected_output.1

### Scenario: -ld test

- Given there is no `src` directory
- Given I run `unzip -q -o 17_Acc/src.zip` Successfully
- Given the file `17_Acc/expected_output.2`
- When I run `./acc -ld -I ./src | sort` Successfully
- Then output matches file 17_Acc/expected_output.2

### Scenario: rules test

- Given there is no `src` directory
- Given I run `unzip -q -o 17_Acc/src.zip` Successfully
- Given the file `17_Acc/archicheck.ac`
- Given the file `17_Acc/expected_output.3`
- When I run `./acc 17_Acc/archicheck.ac -I ./src` Successfully
- Then output matches file 17_Acc/expected_output.3

### Scenario: --list_non_covered

- Given there is no `src` directory
- Given I run `unzip -q -o 17_Acc/src.zip` Successfully
- Given the file `17_Acc/archicheck.ac`
- Given the file `17_Acc/expected_output.4`
- When I run `./acc 17_Acc/archicheck.ac -lnc -I ./src` Successfully
- Then output matches file 17_Acc/expected_output.4
