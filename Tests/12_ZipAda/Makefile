all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=zipada.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2 test3

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "ZipAda code test suite"

	@ # Creating the zipada dir :
	@ unzip -q -o zipada53.zip

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-lf test"

	@ ${TR} cmt "> acc -lf -r -I zip-ada"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 3 expected_output.1 	>> ${MD}
	@ echo '...'					>> ${MD}
	@ tail -n 3 expected_output.1 	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -lf -r -I zip-ada > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-ld test"

	@ ${TR} cmt "> acc -ld -r -I ./zip-ada"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 10 expected_output.2 	>> ${MD}
	@ echo '...'				  	>> ${MD}
	@ tail -n 10 expected_output.2 	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -r -I ./zip-ada | sort > output.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "rules test"

	@ ${TR} cmt "> acc zipadarules.txt -r -I ./zip-ada"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules (not much to test...) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat zipadarules.txt >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ > expected_output.3
	@ ../../obj/acc zipadarules.txt -r -I ./zip-ada > output.3
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* dir? dir?? *.~ zip-ada 
	@ - ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}

