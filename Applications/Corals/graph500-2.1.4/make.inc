# -*- Makefile -*-
# Copyright 2010, Georgia Institute of Technology, USA.
# See COPYING for license.
CFLAGS = -g  -O3 $(PCVS_CFLAGS)
LDLIBS = -lm -lrt $(PCVS_CFLAGS)
CPPFLAGS = $(PCVS_CFLAGS) 

BUILD_OPENMP = Yes
BUILD_MPI=Yes

CFLAGS_OPENMP = -fopenmp

CC=$(PCVS_CC)
CXX=$(PVS_CXX)
