
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

  90 dependencies expected :

```
org.springframework.samples.petclinic.model.BaseEntity class depends on java.io.Serializable
org.springframework.samples.petclinic.model.BaseEntity class depends on javax.persistence.GeneratedValue
org.springframework.samples.petclinic.model.BaseEntity class depends on javax.persistence.GenerationType
org.springframework.samples.petclinic.model.BaseEntity class depends on javax.persistence.Id
org.springframework.samples.petclinic.model.BaseEntity class depends on javax.persistence.MappedSuperclass
org.springframework.samples.petclinic.model.NamedEntity class depends on javax.persistence.Column
org.springframework.samples.petclinic.model.NamedEntity class depends on javax.persistence.MappedSuperclass
org.springframework.samples.petclinic.model.Person class depends on javax.persistence.Column
org.springframework.samples.petclinic.model.Person class depends on javax.persistence.MappedSuperclass
org.springframework.samples.petclinic.model.Person class depends on org.hibernate.validator.constraints.NotEmpty
...
org.springframework.samples.petclinic.vet.Vets class depends on java.util.List
org.springframework.samples.petclinic.vet.Vets class depends on javax.xml.bind.annotation.XmlElement
org.springframework.samples.petclinic.vet.Vets class depends on javax.xml.bind.annotation.XmlRootElement
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
