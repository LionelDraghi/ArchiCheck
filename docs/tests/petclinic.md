
# Spring Pet Clinic code test suite



##  Spring Pet Clinic code test suite / -lf test

  > archicheck -lf -r -I ./src

  Expected (36 files) :

```
src/src/test/java/org/springframework/samples/petclinic/owner/PetTypeFormatterTests.java
src/src/test/java/org/springframework/samples/petclinic/owner/VisitControllerTests.java
src/src/test/java/org/springframework/samples/petclinic/owner/PetControllerTests.java
...
src/src/main/java/org/springframework/samples/petclinic/vet/VetRepository.java
src/src/main/java/org/springframework/samples/petclinic/vet/Vet.java
src/src/main/java/org/springframework/samples/petclinic/vet/VetController.java
```


Spring Pet Clinic code test suite / -lf test [Successful](tests_status.md#successful)

##  Spring Pet Clinic code test suite / -ld test

  > archicheck -ld -r -I spring-petclinic-master

  35 dependencies expected :

```
org.springframework.samples.petclinic.model.ValidatorTests class depends on java.util.Locale
org.springframework.samples.petclinic.model.ValidatorTests class depends on java.util.Set
org.springframework.samples.petclinic.model.ValidatorTests class depends on javax.validation.ConstraintViolation
org.springframework.samples.petclinic.model.ValidatorTests class depends on javax.validation.Validator
org.springframework.samples.petclinic.model.ValidatorTests class depends on org.assertj.core.api.Assertions.assertThat
org.springframework.samples.petclinic.model.ValidatorTests class depends on org.junit.Test
org.springframework.samples.petclinic.model.ValidatorTests class depends on org.springframework.context.i18n.LocaleContextHolder
org.springframework.samples.petclinic.model.ValidatorTests class depends on org.springframework.validation.beanvalidation.LocalValidatorFactoryBean
org.springframework.samples.petclinic.owner.OwnerRepository interface depends on java.util.Collection
org.springframework.samples.petclinic.owner.OwnerRepository interface depends on org.springframework.data.jpa.repository.Query
...
org.springframework.samples.petclinic.vet.VetRepository interface depends on org.springframework.dao.DataAccessException
org.springframework.samples.petclinic.vet.VetRepository interface depends on org.springframework.data.repository.Repository
org.springframework.samples.petclinic.vet.VetRepository interface depends on org.springframework.transaction.annotation.Transactional
org.springframework.samples.petclinic.vet.VetTests class depends on org.assertj.core.api.Assertions.assertThat
org.springframework.samples.petclinic.vet.VetTests class depends on org.junit.Test
org.springframework.samples.petclinic.vet.VetTests class depends on org.springframework.util.SerializationUtils
org.springframework.samples.petclinic.visit.VisitRepository interface depends on java.util.List
org.springframework.samples.petclinic.visit.VisitRepository interface depends on org.springframework.dao.DataAccessException
org.springframework.samples.petclinic.visit.VisitRepository interface depends on org.springframework.data.repository.Repository
org.springframework.samples.petclinic.visit.VisitRepository interface depends on org.springframework.samples.petclinic.model.BaseEntity
```


Spring Pet Clinic code test suite / -ld test [Successful](tests_status.md#successful)

##  Spring Pet Clinic code test suite / rules test

  > archicheck petclinic.ac -r -I ./src

  Rules (not much to test...) :

```
java use is allowed
```

  No error expected


Spring Pet Clinic code test suite / rules test [Successful](tests_status.md#successful)
