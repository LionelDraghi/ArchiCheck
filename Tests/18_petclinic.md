## Feature: Spring Pet Clinic code test suite

### Scenario: -lf test

- Given there is no `src1` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully
- Given I run `mv spring-petclinic-master src1` Successfully
- Given the file `18_Spring_PetClinic/expected_output.1`
- When I run `./acc -lf -r -I src1` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.1

### Scenario: -ld test

- Given there is no `src1` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully
- Given I run `mv spring-petclinic-master src1` Successfully
- Given the file `18_Spring_PetClinic/expected_output.2`
- When I run `./acc -ld -r -I ./src1 | sort` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.2

### Scenario: rules test

- Given there is no `src1` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully
- Given I run `mv spring-petclinic-master src1` Successfully
- Given the file `18_Spring_PetClinic/petclinic.ac`
- When I run `./acc 18_Spring_PetClinic/petclinic.ac -r -I ./src1` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.3

### Scenario: --list_non_covered

- Given there is no `src1` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully
- Given I run `mv spring-petclinic-master src1` Successfully
- Given the file `18_Spring_PetClinic/petclinic.ac`
- When I run `./acc 18_Spring_PetClinic/petclinic.ac -lnc -r -I ./src1` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.4

### Scenario: alternative rules test

- Given there is no `src1` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-petclinic-master.zip` Successfully
- Given I run `mv spring-petclinic-master src1` Successfully
- Given the file `18_Spring_PetClinic/alternative.ac`
- When I run `./acc 18_Spring_PetClinic/alternative.ac -r -I ./src1` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.5

### Scenario: Layered version of petclinic test

- Given there is no `src2` directory
- Given I run `unzip -q -o 18_Spring_PetClinic/spring-framework-petclinic-master.zip` Successfully
- Given I run `mv spring-framework-petclinic-master src2` Successfully
- Given the file `18_Spring_PetClinic/framework-petclinic.ac`
- When I run `./acc 18_Spring_PetClinic/framework-petclinic.ac -r -I ./src2` Successfully
- Then output matches file 18_Spring_PetClinic/expected_output.6
