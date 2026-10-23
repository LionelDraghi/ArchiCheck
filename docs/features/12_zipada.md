# Feature: ZipAda code test suite <!-- omit from toc -->

- [Scenario: -lf test](#scenario--lf-test)
- [Scenario: -ld test](#scenario--ld-test)
- [Scenario: rules test](#scenario-rules-test)


## Scenario: -lf test

- Given there is no `zip-ada` directory
- And I run `unzip -q -o ../docs/features/12_zipada/zipada53.zip` Successfully
- And there is a file `../docs/features/12_zipada/expected_output.1`
- And there is a file `../docs/features/12_zipada/zipadarules.txt`
- When I successfully run `./acc -lf -r -I zip-ada` or `./acc -lf --recursive -I zip-ada`
- Then I get file `../docs/features/12_zipada/expected_output.1`

## Scenario: -ld test

- Given there is no `zip-ada` directory
- And I run `unzip -q -o ../docs/features/12_zipada/zipada53.zip` Successfully
- And there is a file `../docs/features/12_zipada/expected_output.2`
- When I run `./acc -ld -r -I ./zip-ada` Successfully
- Then I get file `../docs/features/12_zipada/expected_output.2` (ignoring order)

## Scenario: rules test

- Given there is no `zip-ada` directory
- And I run `unzip -q -o ../docs/features/12_zipada/zipada53.zip` Successfully
- And there is a file `../docs/features/12_zipada/zipadarules.txt`
- When I run `./acc ../docs/features/12_zipada/zipadarules.txt -r -I ./zip-ada` Successfully
- Then I get no output
