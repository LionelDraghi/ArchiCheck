all: check doc

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4 test5 test6 test7

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Java code test suite"

	@ # Creating the batik dir :
	@ tar -xf batik-src-1.9.tar.gz


test1:
	@ #---------------------------------------------------------------------
	${TR} start "--list_file test, recursive and non recursive"

	@ ${TR} cmt "> archicheck -lf -I ./batik-1.9/test-sources/org/apache/batik/dom"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -lf -I ./batik-1.9/test-sources/org/apache/batik/dom > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} cmt "> archicheck -lf -r -I ./batik-1.9/test-sources/org/apache/batik/dom"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.1b >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -lf -r -I ./batik-1.9/test-sources/org/apache/batik/dom > output.1b
	${TR} assert true /usr/bin/sdiff "-s expected_output.1b output.1b"

	${TR} end


test2:
	@ #---------------------------------------------------------------------
	${TR} start "simple import "

	- @ mkdir -p dir2
	@ cp ./batik-1.9/contrib/jsvg/JSVG.java dir2

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '/package /,/public class/p' dir2/JSVG.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 
	@ ${TR} cmt "> archicheck -lf -I dir2"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -I dir2 > output.2
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "interface"

	- @ mkdir -p dir3
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '/package /,/public interface/p' dir3/NodeEventTarget.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 
	@ ${TR} cmt "> archicheck -lf -I dir3"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.3 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -I dir3 > output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "no import"

	- @ mkdir -p dir4
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '/package /,/public class/p' dir4/TriplyIndexedTable.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt "> archicheck -lf -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.4 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -I dir4 > output.4
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end

test5:
	@ #---------------------------------------------------------------------
	${TR} start "no package"

	- @ mkdir -p dir5
	@ > dir5/MyClass.java
	@ echo "import org.w3c.dom.DOMException;" 			>> dir5/MyClass.java
	@ echo "import org.w3c.dom.events.Event;" 			>> dir5/MyClass.java
	@ echo "" 							>> dir5/MyClass.java
	@ echo "public interface NodeEventTarget extends EventTarget {"	>> dir5/MyClass.java
	@ echo ""				 			>> dir5/MyClass.java
	@ echo "}" 							>> dir5/MyClass.java

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '1,/public interface/p' dir5/MyClass.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt "> archicheck -lf -I dir5"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.5 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../Obj/archicheck -ld -I dir5 > output.5
	${TR} assert true /usr/bin/sdiff "-s expected_output.5 output.5"

	${TR} end

test6:
	@ #---------------------------------------------------------------------
	${TR} start "real Batik code"

	- @ mkdir -p dir6
	@ cp ./batik-1.9/batik-transcoder/src/main/java/org/apache/batik/transcoder/SVGAbstractTranscoder.java dir6

	@ ${TR} cmt 
	@ ${TR} cmt "This class is in transcoder, and uses Bridge and GVT, and that's OK"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '/package /,/public abstract class/p' dir6/SVGAbstractTranscoder.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	> expected_output.6 
	@ ${TR} cmt "> archicheck rules.B -I dir6"
	@ ${TR} cmt "No output expected"

	### @ ../../Obj/archicheck rules.B -I dir6 > output.6
	### ${TR} assert true /usr/bin/sdiff "-s expected_output.6 output.6"

	${TR} end

test7:
	@ #---------------------------------------------------------------------
	${TR} start "Let's add dependencies to Browser and Rasterizer into a Transcoder class"

	- @ mkdir -p dir7
	@ > dir7/MyClass.java
	@ echo "package org.apache.batik.transcoder;"			>> dir7/MyClass.java
	@ echo "" 							>> dir7/MyClass.java
	@ echo "import org.w3c.dom.Browser.Event;" 			>> dir7/MyClass.java
	@ echo "import org.w3c.dom.events.Rasterizer;" 			>> dir7/MyClass.java
	@ echo "" 							>> dir7/MyClass.java
	@ echo "public interface NodeEventTarget extends EventTarget {"	>> dir7/MyClass.java
	@ echo ""				 			>> dir7/MyClass.java
	@ echo "}" 							>> dir7/MyClass.java

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	sed -n '/package /,/public abstract class/p' dir7/MyClass.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt "> archicheck rules.B -I dir7"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	cat expected_output.7 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	### @ ../../Obj/archicheck rules.B -I dir7 > output.7
	### ${TR} assert true /usr/bin/sdiff "-s expected_output.7 output.7"

	${TR} end


clean:
	- ${RM} -rf output.* dir? dir?? *.~ batik-1.9 *.java dir?
	- ${TR} clean

doc:
	cp testrec.md ../../docs/tests/batik.md
