# On Windows the executable carries an .exe suffix
ifeq ($(OS), Windows_NT)
	EXE_SUFFIX := .exe
else
	EXE_SUFFIX :=
endif

all: create_pkg

create_pkg:
	alr build --release

.PHONY : clean
clean:
	- ${RM} create_pkg$(EXE_SUFFIX) obj/*
