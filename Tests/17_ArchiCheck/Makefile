all: check doc

CREATE_PKG=../../Tools/create_pkg
MD=archicheck.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2 test3 test4

.PHONY : header
header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "ArchiCheck code test suite"
	@ - mkdir -p ./src
	@ cp -rp ../../src/*.ad? src
	
test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-lf test"

	@ ls src/*.ad? | sort > expected_output.1

	@ CMD="acc -lf -I src "

	@ ${TR} cmt 'Executed:'
	@ ${TR} cmt 
	@ ${TR} cmt '> acc -lf -I src'
	@ ${TR} cmt 
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 3 expected_output.1 >> ${MD}
	@ echo '...'				  >> ${MD}
	@ tail -n 3 expected_output.1 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -lf -I src | sort > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end

test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-ld test"

	@ ${TR} cmt "> acc -ld -I ./src | sort"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.2 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 10 expected_output.2 >> ${MD}
	@ echo '...'				   >> ${MD}
	@ tail -n 10 expected_output.2 >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I ./src | sort > output.2
 
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "rules test"

	@ ${TR} cmt "> acc archicheck.ac -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat archicheck.ac >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "![ArchiCheck dependencies view](ac_view.png)"
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ > expected_output.3
	@ ../../obj/acc archicheck.ac -I ./src > output.3
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "--list_non_covered"

	@ ${TR} cmt "> acc archicheck.ac -lnc -I ./src"
	@ ${TR} cmt 
	@ ${TR} cmt "Rules :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat archicheck.ac >> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "![ArchiCheck dependencies view](ac_view.png)"
	@ ${TR} cmt 
	@ ${TR} cmt "No error expected"
	@ ${TR} cmt 

	@ ../../obj/acc archicheck.ac -lnc -I ./src > output.4
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.4 output.4"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* src ac_view.png
	@ - ${TR} clean

doc:
	@ deplo config=archicheck.deplo | dot -Tpng -oac_view.png
	@ cp ${MD} ../../docs/tests/${MD}
	@ cp ac_view.png ../../docs/tests/


