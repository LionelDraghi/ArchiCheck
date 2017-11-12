all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 test2 test3 test4 test5

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Java code tests"

	@ # Creating the batik dir :
	@ tar -xf batik-src-1.9.tar.gz


test1:
	@ #---------------------------------------------------------------------
	${TR} start "--list_file test, recursive and non recursive"

	@ ${TR} cmt "> archicheck -lf -I ./batik-1.9/test-sources/org/apache/batik/dom"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.1 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -lf -I ./batik-1.9/test-sources/org/apache/batik/dom > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} cmt "> archicheck -lf -r -I ./batik-1.9/test-sources/org/apache/batik/dom"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.1b >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -lf -r -I ./batik-1.9/test-sources/org/apache/batik/dom > output.1b
	${TR} assert true /usr/bin/sdiff "-s expected_output.1b output.1b"

	${TR} end


test2:
	@ #---------------------------------------------------------------------
	${TR} start "simple import "

	- @ mkdir dir2
	@ cp ./batik-1.9/contrib/jsvg/JSVG.java dir2

	@ ${TR} cmt "(start code)"
	sed -n '/package /,/public class/p' dir2/JSVG.java >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ${TR} cmt "> archicheck -lf -I dir2"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.2 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -ld -I dir2 > output.2
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "interface"

	- @ mkdir dir3
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3

	@ ${TR} cmt "(start code)"
	sed -n '/package /,/public interface/p' dir3/NodeEventTarget.java >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ${TR} cmt "> archicheck -lf -I dir3"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.3 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -ld -I dir3 > output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "no import"

	- @ mkdir dir4
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4

	@ ${TR} cmt "(start code)"
	sed -n '/package /,/public class/p' dir4/TriplyIndexedTable.java >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ${TR} cmt "> archicheck -lf -I dir4"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.4 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -ld -I dir4 > output.4
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end

test5:
	@ #---------------------------------------------------------------------
	${TR} start "no package"

	- @ mkdir dir5
	@ > dir5/MyClass.java
	@ echo "import org.w3c.dom.DOMException;" 			>> dir5/MyClass.java
	@ echo "import org.w3c.dom.events.Event;" 			>> dir5/MyClass.java
	@ echo "" 							>> dir5/MyClass.java
	@ echo "public interface NodeEventTarget extends EventTarget {"	>> dir5/MyClass.java
	@ echo ""				 			>> dir5/MyClass.java
	@ echo "}" 							>> dir5/MyClass.java

	@ ${TR} cmt "(start code)"
	sed -n '/package /,/public class/p' dir5/MyClass.java >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ${TR} cmt "> archicheck -lf -I dir5"
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt "(start code)"
	cat expected_output.5 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -ld -I dir5 > output.5
	${TR} assert true /usr/bin/sdiff "-s expected_output.5 output.5"

	${TR} end

testx:
	@ #---------------------------------------------------------------------
	${TR} start "import with wild card"

	@ cp ./batik-1.9/contrib/scroll/ScrollExample.java .

	@ ${TR} cmt "(start code)"
	sed -n '/package /,/public class/p' ScrollExample.java >> testrec.txt
	@ ${TR} cmt "(end)"

	@ > expected_output.1
	@ ../../Obj/archicheck -ld -I . rules.1 > output.1
	${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	${TR} end


Batik:
	@ #---------------------------------------------------------------------
	${TR} start "Batik project architecture, code is compliant with rules file, no output expected"
	@ ${CREATE_PKG} Browser       spec -in dirB -with UI_Component
	@ ${CREATE_PKG} Rasterizer    spec -in dirB -with Transcoder
	@ ${CREATE_PKG} UI_Component  spec -in dirB -with Bridge -with Renderer
	@ ${CREATE_PKG} Transcoder    spec -in dirB -with Bridge -with Renderer
	@ ${CREATE_PKG} Bridge        spec  -in dirB -with GVT -with SVGDOM
	@ ${CREATE_PKG} Renderer      spec  -in dirB -with GVT
	@ ${CREATE_PKG} GVT           spec  -in dirB
	@ ${CREATE_PKG} SVGDOM        spec  -in dirB -with SVG_Parser
	@ ${CREATE_PKG} SVG_Parser    spec  -in dirB
	@ ${CREATE_PKG} SVG_Generator spec  -in dirB -with SVGDOM

	@ ${TR} cmt "Architecture of the The Apache Batik Project, available <here at https://xmlgraphics.apache.org/batik/using/architecture.html>".
	@ ${TR} cmt "(see Batik.png)"
	@ ${TR} cmt "Described by this rules file :"
	@ ${TR} cmt "(start code)"
	cat rules.B >> testrec.txt
	@ ${TR} cmt "(end)"

	@ > expected_output.B
	@ ../../Obj/archicheck -q -I dir1 rules.B > output.B
	${TR} assert true /usr/bin/sdiff "-s expected_output.B output.B"

	${TR} end


clean:
	- ${RM} -rf output.* dir? dir?? *.~ batik-1.9 *.java dir?
	- ${TR} clean

