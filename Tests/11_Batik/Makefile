all: check doc

CREATE_PKG=../../Tools/create_pkg
TR=../../Tools/testrec

.PHONY : clean header

check: ../../obj/acc header test1 test2 test3 test4 test5 test6 test7 test8

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "Batik test suite"

	@ # Creating the batik dir :
	@ tar -xf batik-src-1.9.tar.gz


test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start "--list_file test"

	@ ${TR} cmt "> acc -lf -Ir ./batik-1.9"
	@ ${TR} cmt "Expected (`cat expected_output.1 | wc -l` files) :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 3 expected_output.1 >> testrec.md
	@ echo '...'					>> testrec.md
	@ tail -n 3 expected_output.1 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -lf -Ir ./batik-1.9 | sort > output.1
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.1 output.1"

	@ ${TR} end


test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "public class"

	@ - @ mkdir -p dir2
	@ cp ./batik-1.9/contrib/jsvg/JSVG.java dir2

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ sed -n '/package /,/public class/p' dir2/JSVG.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 
	@ ${TR} cmt "> acc -ld -I dir2"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.2 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I dir2 > output.2
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.2 output.2"

	@ ${TR} end

test3:
	@ #---------------------------------------------------------------------
	@ ${TR} start "public interface class"

	@ - @ mkdir -p dir3
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/events/NodeEventTarget.java dir3

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ sed -n '/package /,/public interface/p' dir3/NodeEventTarget.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 
	@ ${TR} cmt "> acc -ld -I dir3"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.3 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I dir3 > output.3
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.3 output.3"

	@ ${TR} end

test4:
	@ #---------------------------------------------------------------------
	@ ${TR} start "no import"

	@ - @ mkdir -p dir4
	@ cp ./batik-1.9/batik-dom/src/main/java/org/apache/batik/dom/util/TriplyIndexedTable.java dir4

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ sed -n '/package /,/public class/p' dir4/TriplyIndexedTable.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt "> acc -ld -I dir4"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.4 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I dir4 > output.4
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.4 output.4"

	@ ${TR} end

test5:
	@ #---------------------------------------------------------------------
	@ ${TR} start "no package"

	@ - @ mkdir -p dir5
	@ > dir5/MyClass.java
	@ echo "import org.w3c.dom.DOMException;" 						>> dir5/MyClass.java
	@ echo "import org.w3c.dom.events.Event;" 						>> dir5/MyClass.java
	@ echo "" 														>> dir5/MyClass.java
	@ echo "public interface NodeEventTarget extends EventTarget {"	>> dir5/MyClass.java
	@ echo ""				 										>> dir5/MyClass.java
	@ echo "}" 														>> dir5/MyClass.java

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ sed -n '1,/public interface/p' dir5/MyClass.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt "> acc -ld -I dir5"
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.5 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I dir5 > output.5
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.5 output.5"

	@ ${TR} end

test6:
	@ #---------------------------------------------------------------------
	@ ${TR} start "public abstract class"

	@ - @ mkdir -p dir6
	@ cp ./batik-1.9/batik-transcoder/src/main/java/org/apache/batik/transcoder/SVGAbstractTranscoder.java dir6

	@ ${TR} cmt 
	@ ${TR} cmt "This class is in transcoder, and uses Bridge and GVT, and that's OK"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ sed -n '/package /,/public abstract class/p' dir6/SVGAbstractTranscoder.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ > expected_output.6 
	@ ${TR} cmt "> acc rules.B -q -I dir6"
	@ ${TR} cmt "No output expected"

	@ ../../obj/acc rules.B -q -I dir6 > output.6
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.6 output.6"

	@ ${TR} end

test7:
	@ #---------------------------------------------------------------------
	@ ${TR} start "Let's add dependencies to Browser and Rasterizer into a Transcoder class"

	@ - @ mkdir -p dir7
	@ > dir7/MyClass.java
	@ echo "package org.apache.batik.transcoder;"					>> dir7/MyClass.java
	@ echo "" 														>> dir7/MyClass.java
	@ echo "import org.w3c.dom.Browser.Event;" 						>> dir7/MyClass.java
	@ echo "import org.apache.batik.apps.Rasterizer;" 	  			>> dir7/MyClass.java
	@ echo "" 														>> dir7/MyClass.java
	@ echo "public interface NodeEventTarget extends EventTarget {"	>> dir7/MyClass.java
	@ echo ""				 										>> dir7/MyClass.java
	@ echo "}" 														>> dir7/MyClass.java

	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat dir7/MyClass.java >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 'Rules:'
	@ ${TR} cmt '```'
	@ cat rules.7 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ${TR} cmt 'Run:'
	@ ${TR} cmt "> acc rules.7 -I dir7"
	@ ${TR} cmt 
	@ ${TR} cmt 'Expected:'
	@ ${TR} cmt '```'
	@ cat expected_output.7 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ test ! `../../obj/acc rules.7 -I dir7 > output.7`
	@ ${TR} assert true /usr/bin/sdiff "-s expected_output.7 output.7"

	@ ${TR} end

test8:
	@ #---------------------------------------------------------------------
	@ ${TR} start "-ld test"

	@ ${TR} cmt "> acc -ld -Ir ./batik-1.9 | sort"
	@ ${TR} cmt 
	@ ${TR} cmt "`cat expected_output.8 | wc -l` dependencies expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ head -n 10 expected_output.8 >> testrec.md
	@ echo '...'				  	 >> testrec.md
	@ tail -n 10 expected_output.8 >> testrec.md
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -Ir ./batik-1.9 | sort > output.8
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.8 output.8"

	@ ${TR} end

clean:
	@ - ${RM} -rf output.* dir? dir?? *.~ batik-1.9 *.java dir?
	@ - ${TR} clean

doc:
	@ cp testrec.md ../../docs/tests/batik.md
