all: check

CREATE_PKG=../Tools/create_pkg
TR=../Tools/testrec

.PHONY : clean

check: ../../Obj/archicheck header test1 Batik test2 test3 test4

header:
	@ #---------------------------------------------------------------------
	${TR} clean
	${TR} create "Layer rules test suite"


test1:
	@ #---------------------------------------------------------------------
	${TR} start "Code is compliant with rules file, no output expected"
	@ ${CREATE_PKG} P1 spec  -in dir1 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir1 -with P3
	@ ${CREATE_PKG} P2 body  -in dir1 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir1
	@ ${CREATE_PKG} P4 spec  -in dir1

	@ ${TR} cmt "(see test_1.png)"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ > expected_output.1
	@ ../../Obj/archicheck -q -I dir1 rules.1 > output.1
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

test2:
	@ #---------------------------------------------------------------------
	${TR} start "Reverse dependency test"
	@ ${CREATE_PKG} P1 spec  -in dir2 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir2 -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir2
	@ ${CREATE_PKG} P4 spec  -in dir2 -with P5
	@ ${CREATE_PKG} P5 spec  -in dir2

	@ ${TR} cmt "Detection of a dependancy from a lower layer component to an upper layer component."
	@ ${TR} cmt "(see test_2.png)"
	@ ${TR} cmt "(start code)"
	cat rules.2 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.2 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck --quiet -I dir2 rules.2 > output.2
	${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	${TR} end

test3:
	@ #---------------------------------------------------------------------
	${TR} start "Layer bridging test"
	@ ${CREATE_PKG} P1 spec  -in dir3a -with P2
	@ ${CREATE_PKG} P2 spec  -in dir3a
	@ ${CREATE_PKG} P2 body  -in dir3a -with P3 -with P4
	@ ${CREATE_PKG} P3 spec  -in dir3b
	@ ${CREATE_PKG} P4 spec  -in dir3b
	@ ${CREATE_PKG} P6 spec  -in dir3c
	@ ${CREATE_PKG} P6 body  -in dir3c -with P4

	@ ${TR} cmt "Detection of a dependancy link crossing a layer."
	@ ${TR} cmt "(see test_3.png)"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.3 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -I dir3a -I dir3b -I dir3c rules.1 > output.3
	${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	${TR} end

test4:
	@ #---------------------------------------------------------------------
	${TR} start "Using a package that is neither in the same layer, nor in the visible layer"
	@ ${CREATE_PKG} P1 spec  -in dir4 -with P2
	@ ${CREATE_PKG} P2 spec  -in dir4 -with P3 -with P4 -with P7
	@ ${CREATE_PKG} P3 spec  -in dir4
	@ ${CREATE_PKG} P4 spec  -in dir4
	@ ${CREATE_PKG} P7 spec  -in dir4

	@ ${TR} cmt "Detection of an undescribed dependancy to a component that is neither in the same layer, nor in the lower layer."
	@ ${TR} cmt "(see test_4.png)"
	@ ${TR} cmt "(start code)"
	cat rules.1 >> testrec.txt
	@ ${TR} cmt "(end)"
	@ ${TR} cmt
	@ ${TR} cmt "Expected output :"
	@ ${TR} cmt "(start code)"
	cat expected_output.4 >> testrec.txt
	@ ${TR} cmt "(end)"

	@ ../../Obj/archicheck -I dir4 rules.1 > output.4
	${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	${TR} end

clean:
	- ${RM} -rf output.* dir? dir?? *.~
	${TR} clean

