all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Spring Pet Clinic code test suite"

	@ unzip -q -o spring-petclinic-master.zip
	- rm -rf src
	mv --no-target-directory spring-petclinic-master src
	# --no-target-directory : traiter la CIBLE comme un fichier normal
	# (to avoid creating src/src if src exists)

test1:
	@ #---------------------------------------------------------------------
	${TR} start "-lf test"

	@ ${TR} cmt "> archicheck -lf -r -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 3 expected_output.1 >> testrec.md
	echo '...'					>> testrec.md
	tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -lf -r -I ./src > output.1
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	${TR} end

test2:
	@ #---------------------------------------------------------------------
	${TR} start "-ld test"

	@ ${TR} cmt "> archicheck -ld -r -I spring-petclinic-master"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	head -n 10 expected_output.2 >> testrec.md
	echo '...'				  	 >> testrec.md
	tail -n 10 expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -r -I ./src | sort > output.2
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "rules test"

	@ ${TR} cmt "> archicheck petclinic.ac -r -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules (not much to test...) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat petclinic.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	> expected_output.3
	@ ../../Obj/archicheck petclinic.ac -r -I ./src > output.3
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "--list_non_covered"

	@ ${TR} cmt "> archicheck petclinic.ac -lnc -r -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat petclinic.ac >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	../../Obj/archicheck petclinic.ac -lnc -r -I ./src > output.4
	${TR} assert true /usr/bin/sdiff "-sbB expected_output.4 output.4"

	${TR} end

clean:
	- ${RM} -rf output.* *.~ src
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/petclinic.md

