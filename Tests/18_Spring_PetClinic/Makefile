all: check doc

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

.PHONY : clean

check: ../../obj/acc header test1 test2 test3 test4 test5 test6

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Spring Pet Clinic code test suite"

	@ unzip -q -o spring-petclinic-master.zip
	@ unzip -q -o spring-framework-petclinic-master.zip
	@ - rm -rf src?
	@ mv --no-target-directory spring-petclinic-master src1
	@ mv --no-target-directory spring-framework-petclinic-master src2
	@ # --no-target-directory : traiter la CIBLE comme un fichier normal
	@ # (to avoid creating src/src if src exists)

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-lf test"

	@ ${TR} cmt "> acc -lf -r -I src1"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 3 expected_output.1 >> testrec.md
	@ echo '...'					>> testrec.md
	@ tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -lf -r -I src1 > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-ld test"

	@ ${TR} cmt "> acc -ld -r -I spring-petclinic-master"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 10 expected_output.2 >> testrec.md
	@ echo '...'				  	 >> testrec.md
	@ tail -n 10 expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -r -I ./src1 | sort > output.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "rules test"

	@ ${TR} cmt "> acc petclinic.ac -r -I ./src1"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat petclinic.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ > expected_output.3
	@ ../../obj/acc petclinic.ac -r -I ./src1 > output.3
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "--list_non_covered"

	@ ${TR} cmt "> acc petclinic.ac -lnc -r -I ./src1"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat petclinic.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ ../../obj/acc petclinic.ac -lnc -r -I ./src1 > output.4
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.4 output.4"

	@ ${TR} end

test5:
	@ #---------------------------------------------------------------------
	@ ${TR} start "alternative rules test"

	@ ${TR} cmt "> acc alternative.ac -r -I ./src1"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat alternative.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ > expected_output.5
	@ ../../obj/acc alternative.ac -r -I ./src1 > output.5
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.5 output.5"

	@ ${TR} end

test6:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Layered version of petclinic test"
	
	@ ${TR} cmt 
	@ ${TR} cmt "[](https://github.com/spring-petclinic/spring-framework-petclinic)"
	@ ${TR} cmt 
	@ ${TR} cmt "> acc framework-petclinic.ac -r -I ./src2"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat framework-petclinic.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ > expected_output.6
	@ @ ../../obj/acc framework-petclinic.ac -r -I ./src2 > output.6
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.6 output.6"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* *.~ src?
	@ - ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/petclinic.md

