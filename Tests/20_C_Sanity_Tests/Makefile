all: check doc example

CREATE_PKG=../../Tools/create_pkg
MD=C_sanity_check.md
TR=../../Tools/testrec -o ${MD} 

.PHONY : clean

check: ../../obj/acc header test1 test2


# Makefile and example provided by [LucasJ](https://stackoverflow.com/users/18600540/lucasj)
# in https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code
SRCS := main.c square_root.c newton_method.c
BIN := main
SRCS := $(SRCS:%=src/%)
OBJS := $(SRCS:src/%.c=obj/%.o)
DEPS := $(OBJS:%.o=%.d)

example: $(BIN)

$(BIN): $(OBJS)
	gcc -o $@ $^

obj/%.o: src/%.c obj/%.d
	gcc -c -I include -MMD -MP -MF $(patsubst obj/%.o,obj/%.d,$@) -MT $@ -o $@ $<

$(DEPS):
	-include $(DEPS)

header:
	@ #---------------------------------------------------------------------
	@ ${TR} clean
	@ ${TR} create "C sanity test suite"

test1:
	@ #---------------------------------------------------------------------
	@ ${TR} start ".c and .h files list"

	@ ${TR} cmt 
	@ ${TR} cmt "Sources :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ls -1 src	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "Includes :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ ls -1 include	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When :"
	@ ${TR} cmt 
	@ ${TR} cmt "> -lf -I include -I src "
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.1	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -lf -I include -I src > output.1
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.1 output.1"

	@ ${TR} end


test2:
	@ #---------------------------------------------------------------------
	@ ${TR} start "dependencies list"

	@ ${TR} cmt 
	@ ${TR} cmt "obj/*.d files :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat obj/*.d	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 
	@ ${TR} cmt "When :"
	@ ${TR} cmt 
	@ ${TR} cmt "> acc -ld -I include -I src "
	@ ${TR} cmt 
	@ ${TR} cmt "Expected :"
	@ ${TR} cmt 
	@ ${TR} cmt '```'
	@ cat expected_output.2	>> ${MD}
	@ ${TR} cmt '```'
	@ ${TR} cmt 

	@ ../../obj/acc -ld -I include -I src | sort > output.2
	@ ${TR} assert true /usr/bin/sdiff "-sbB expected_output.2 output.2"

	@ ${TR} end


clean:
	@ - ${RM} -rf output.* *.~ dir?
	@ - ${TR} clean

doc:
	@ cp ${MD} ../../docs/tests/${MD}
	

