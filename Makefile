# Makefile for Bin2Data

ifeq ($(OS),)
	ifeq ($(shell uname -s),Linux)
		OS := Linux
	endif
	ifeq ($(shell uname -s),Darwin)
		OS := macOS
	endif
endif

ifeq ($(OS),)
	OS := Windows_NT
endif

ifeq ($(filter $(OS),Linux macOS Windows_NT),)
	$(error OS must be set to 'Linux', 'macOS', or 'Windows_NT'.)
endif

$(info OS: $(OS))

ifeq ($(OS),Windows_NT)
	EXTENSION := .exe
	ifeq ($(shell where rm 2>nul),)
		RM := del /q /f
		FIXPATH = $(subst /,\,$1)
	else
		RM := rm -f
		FIXPATH = $1
	endif
else
	RM := rm -fr
	EXTENSION :=
	FIXPATH = $1
endif

SEARCH_PATHS := $(QB64PE_PATH) . ../qb64pe ../QB64pe ../QB64PE

QB64PE_PATH_FOUND = $(firstword $(foreach dir,$(SEARCH_PATHS),$(if $(wildcard $(dir)/qb64pe$(EXTENSION)),$(dir),)))

ifneq ($(QB64PE_PATH_FOUND),)
	QB64PE ?= $(QB64PE_PATH_FOUND)/qb64pe$(EXTENSION)
else
	QB64PE ?= qb64pe$(EXTENSION)
endif

$(info Using QB64PE from: $(QB64PE))

QB64PE_FLAGS := -x -w -e

APP_EXECUTABLE := Bin2Data$(EXTENSION)

SOURCES := source/Bin2Data.bas
INCLUDES := $(wildcard source/*.bi)
RESOURCES := source/Bin2Data.ico

.PHONY: all release test clean

all: $(APP_EXECUTABLE)

release: clean
	$(MAKE) all QB64PE_FLAGS="$(QB64PE_FLAGS) -f:OptimizeCppProgram=true"

$(APP_EXECUTABLE): $(SOURCES) $(INCLUDES) $(RESOURCES)
	$(QB64PE) $(QB64PE_FLAGS) $< -o $@

test: all
ifeq ($(OS),Windows_NT)
	powershell.exe -NoProfile -ExecutionPolicy Bypass -File tests/tests.ps1
else
	chmod +x tests/tests.sh
	./tests/tests.sh
endif

clean:
	-$(RM) $(call FIXPATH,$(APP_EXECUTABLE)) $(call FIXPATH,source/$(APP_EXECUTABLE))
