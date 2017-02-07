#BHEADER**********************************************************************
# Copyright (c) 2006   The Regents of the University of California.
# Produced at the Lawrence Livermore National Laboratory.
# Written by the HYPRE team, UCRL-CODE-222953.
# All rights reserved.
#
# This file is part of HYPRE (see http://www.llnl.gov/CASC/hypre/).
# Please see the COPYRIGHT_and_LICENSE file for the copyright notice, 
# disclaimer and the GNU Lesser General Public License.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License (as published by the Free
# Software Foundation) version 2.1 dated February 1999.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the IMPLIED WARRANTY OF MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the terms and conditions of the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
#
# $Revision: 2.21 $
#EHEADER**********************************************************************


.SUFFIXES:
.SUFFIXES: .c .f .o
                                                                                               
srcdir = .
                                                                                               
include ../Makefile.include
                                                                                               
CFLAGS =\
 -I..\
 ${INCLUDE_CFLAGS}\
 -DHYPRE_TIMING
                                                                                               
RANLIB= ranlib


##################################################################
# Targets
##################################################################

all: libHYPRE_utilities.a

clean:
	@rm -f *.o 

veryclean: clean
	@rm -f libHYPRE_*.a

##################################################################
# Utilities rules
##################################################################

UTILITIES_HEADERS =\
 amg_linklist.h\
 exchange_data.h\
 general.h\
 hypre_memory.h\
 hypre_smp_forloop.h\
 HYPRE_utilities.h\
 mpistubs.h\
 threading.h\
 thread_mpistubs.h\
 timing.h\
 umalloc_local.h\
 utilities.h

UTILITIES_FILES =\
 amg_linklist.c\
 binsearch.c\
 exchange_data.c\
 hypre_memory.c\
 hypre_qsort.c\
 memory_dmalloc.c\
 mpistubs.c\
 qsplit.c\
 random.c\
 threading.c\
 thread_mpistubs.c\
 timer.c\
 timing.c\
 umalloc_local.c\
 hypre_error.c

UTILITIES_OBJS = ${UTILITIES_FILES:.c=.o}

 
libHYPRE_utilities.a: ${UTILITIES_OBJS}
	@echo  "Building $@ ... "
	ar -rcu $@ ${UTILITIES_OBJS}
	${RANLIB} $@

${UTILITIES_OBJS}: ${UTILITIES_HEADERS}

timer.o: timer.c
	${CC} -D_POSIX_SOURCE -o $@ -c ${CFLAGS} $<

##################################################################
# Generic rules
##################################################################

.c.o:
	${CC} -o $@ -c ${CFLAGS} $<
