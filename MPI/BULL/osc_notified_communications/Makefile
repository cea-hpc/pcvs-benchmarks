#
#     (C) 2019 by Bull S. A. S.  All rights reserved.
#

CC=mpicc
override CFLAGS+= -fPIC -Werror -Wextra
#CFLAGS= -fPIC -Wall -Werror -Wextra -Wfatal-errors

DIRNAME=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
LIBS=-L$(DIRNAME)/.libs -lcommon -Wl,-rpath=$(DIRNAME)/.libs
BINARIES=$(DIRNAME)/bin
INCLUDES=-I$(DIRNAME)

SRC_TEST= $(wildcard $(DIRNAME)/error/*.c) 			\
			$(wildcard $(DIRNAME)/functional/*.c)	\
			$(wildcard $(DIRNAME)/performance/*.c)

SRC_COMMON=$(wildcard $(DIRNAME)/util/*.c)
INCL_COMMON=$(wildcard $(DIRNAME)/util/*.h)

OBJ_TEST=$(SRC_TEST:.c=.o)
EXEC_TEST=$(SRC_TEST:.c=)
OBJ_COMMON=$(SRC_COMMON:.c=.o)

INSTALL_PREFIX?=$(DIRNAME)/build

all: $(EXEC_TEST)

$(EXEC_TEST): $(OBJ_TEST) common
	mkdir -p $(DIRNAME)/bin
	$(CC) -o $(DIRNAME)/bin/$(shell basename $@) $@.o $(LIBS)

common: $(OBJ_COMMON)
	mkdir -p $(DIRNAME)/.libs
	$(CC) $(INCL_COMMON) -shared -o $(DIRNAME)/.libs/libcommon.so $^

%.o: %.c
	$(CC) -g -o $@ -c $< $(CFLAGS) $(INCLUDES)

clean:
	\rm -rf $(DIRNAME)/$(EXEC_TEST) \
			$(DIRNAME)/error/*.o \
			$(DIRNAME)/functional/*.o \
			$(DIRNAME)/performance/*.o \
			$(DIRNAME)/util/*.o \
			$(DIRNAME)/.libs \
			$(DIRNAME)/bin

install: all
	mkdir -p $(INSTALL_PREFIX)
	install -t $(INSTALL_PREFIX) $(BINARIES)/*
