# On Windows the executable carries an .exe suffix
ifeq ($(OS), Windows_NT)
	EXE_SUFFIX := .exe
else
	EXE_SUFFIX :=
endif

all: create_pkg

.PHONY : help
help:
	@ echo "Usage: make [target]"
	@ echo ""
	@ echo "Targets:"
	@ echo "  all         : create_pkg (default when no target given)"
	@ echo "  create_pkg  : build create_pkg in release mode"
	@ echo "  clean       : remove the binary and the objects"

create_pkg:
	alr build --release

.PHONY : clean
clean:
	- ${RM} create_pkg$(EXE_SUFFIX) obj/*
