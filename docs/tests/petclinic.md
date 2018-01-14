
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

```
java use is allowed
javax use is allowed
org.springframework use is allowed
org.hibernate.validator use is allowed

-- This organisation in layer is described here : http://tidyjava.com/layered-architecture-good/

Domain contains org.springframework.samples.petclinic.model.NamedEntity
Domain contains org.springframework.samples.petclinic.owner
Domain contains org.springframework.samples.petclinic.model.Person
Domain contains org.springframework.samples.petclinic.owner.PetValidator

Infrastructure contains org.springframework.samples.petclinic.model.BaseEntity
Infrastructure contains org.springframework.samples.petclinic.owner.OwnerRepository
Infrastructure contains org.springframework.samples.petclinic.owner.PetRepository
Infrastructure contains org.springframework.samples.petclinic.visit.VisitRepository
Infrastructure contains org.springframework.samples.petclinic.vet.VetRepository

Presentation contains org.springframework.samples.petclinic.owner.PetTypeFormatter
Presentation contains org.springframework.samples.petclinic.owner.VisitController
Presentation contains org.springframework.samples.petclinic.owner.OwnerController
Presentation contains org.springframework.samples.petclinic.system.WelcomeController
Presentation contains org.springframework.samples.petclinic.system.CrashController
Presentation contains org.springframework.samples.petclinic.vet.VetController
Presentation contains org.springframework.samples.petclinic.vet.Vets

Presentation is a layer over Domain
Domain       is a layer over Infrastructure

-- Application layer is normally between Presentation and Domain layer.
-- But commented out because empty in this case, to avoid the warning.
-- Application  is a layer over Domain 
```

  No error expected


Spring Pet Clinic code test suite / rules test [Successful](tests_status.md#successful)

##  Spring Pet Clinic code test suite / --list_non_covered

  > archicheck petclinic.ac -lnc -r -I ./src

  Rules :

```
java use is allowed
javax use is allowed
org.springframework use is allowed
org.hibernate.validator use is allowed

-- This organisation in layer is described here : http://tidyjava.com/layered-architecture-good/

Domain contains org.springframework.samples.petclinic.model.NamedEntity
Domain contains org.springframework.samples.petclinic.owner
Domain contains org.springframework.samples.petclinic.model.Person
Domain contains org.springframework.samples.petclinic.owner.PetValidator

Infrastructure contains org.springframework.samples.petclinic.model.BaseEntity
Infrastructure contains org.springframework.samples.petclinic.owner.OwnerRepository
Infrastructure contains org.springframework.samples.petclinic.owner.PetRepository
Infrastructure contains org.springframework.samples.petclinic.visit.VisitRepository
Infrastructure contains org.springframework.samples.petclinic.vet.VetRepository

Presentation contains org.springframework.samples.petclinic.owner.PetTypeFormatter
Presentation contains org.springframework.samples.petclinic.owner.VisitController
Presentation contains org.springframework.samples.petclinic.owner.OwnerController
Presentation contains org.springframework.samples.petclinic.system.WelcomeController
Presentation contains org.springframework.samples.petclinic.system.CrashController
Presentation contains org.springframework.samples.petclinic.vet.VetController
Presentation contains org.springframework.samples.petclinic.vet.Vets

Presentation is a layer over Domain
Domain       is a layer over Infrastructure

-- Application layer is normally between Presentation and Domain layer.
-- But commented out because empty in this case, to avoid the warning.
-- Application  is a layer over Domain 
```

  No error expected


Spring Pet Clinic code test suite / --list_non_covered [Successful](tests_status.md#successful)

##  Spring Pet Clinic code test suite / alternative rules test

  > archicheck 	alternative.ac -r -I ./src

```
java use is allowed
javax use is allowed
-- org.springframework use is allowed
org.hibernate.validator use is allowed

-- No definition of a model Model component, direct use in rule :
org.springframework.samples.petclinic.model may use Org.SpringFramework

-- Definition of Owner using a kind of rename :
Owner contains org.springframework.samples.petclinic.owner

-- Definition of System using individual "contains" :
System contains org.springframework.samples.petclinic.system.WelcomeController
System contains org.springframework.samples.petclinic.system.CrashController

Vet contains org.springframework.samples.petclinic.vet.VetRepository
Vet contains org.springframework.samples.petclinic.vet.VetController
Vet contains org.springframework.samples.petclinic.vet.Vets

Visit contains org.springframework.samples.petclinic.visit.VisitRepository

Owner may use org.springframework.samples.petclinic.model
Owner  may use Org.SpringFramework
System may use Org.SpringFramework
Org.SpringFramework may use System 
```

  No error expected


Spring Pet Clinic code test suite / alternative rules test [Successful](tests_status.md#successful)
