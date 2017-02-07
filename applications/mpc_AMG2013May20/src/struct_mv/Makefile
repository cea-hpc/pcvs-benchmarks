#BHEADER**********************************************************************
# Copyright (c) 2006   The Regents of the University of California.
# Produced at the Lawrence Livermore National Laboratory.
# Written by the HYPRE team. UCRL-CODE-222953.
# All rights reserved.
#
# This file is part of HYPRE (see http://www.llnl.gov/CASC/hypre/).
# Please see the COPYRIGHT_and_LICENSE file for the copyright notice, 
# disclaimer, contact information and the GNU Lesser General Public License.
#
# HYPRE is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License (as published by the Free Software 
# Foundation) version 2.1 dated February 1999.
#
# HYPRE is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the IMPLIED WARRANTY OF MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE.  See the terms and conditions of the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
#
# $Revision: 1.1 $
#EHEADER**********************************************************************

                                                             
.SUFFIXES:
.SUFFIXES: .c .f .o
                                                                                               
srcdir = .
                                                                                               
include ../Makefile.include
                                                                                               
CFLAGS =\
 -I..\
 -I../utilities\
 ${INCLUDE_CFLAGS}
                                                                                               
RANLIB = ranlib
                                                                                               
HEADERS =\
 assumed_part.h\
 box.h\
 box_manager.h\
 box_neighbors.h\
 box_pthreads.h\
 computation.h\
 headers.h\
 hypre_box_smp_forloop.h\
 HYPRE_struct_mv.h\
 struct_communication.h\
 struct_grid.h\
 struct_matrix.h\
 struct_mv.h\
 struct_stencil.h\
 struct_vector.h

FILES =\
 assumed_part.c\
 box_algebra.c\
 box_alloc.c\
 box_boundary.c\
 box.c\
 box_manager.c\
 box_neighbors.c\
 communication_info.c\
 computation.c\
 grow.c\
 HYPRE_struct_grid.c\
 HYPRE_struct_matrix.c\
 HYPRE_struct_stencil.c\
 HYPRE_struct_vector.c\
 new_assemble.c\
 new_box_neighbors.c\
 project.c\
 struct_axpy.c\
 struct_communication.c\
 struct_copy.c\
 struct_grid.c\
 struct_innerprod.c\
 struct_io.c\
 struct_matrix.c\
 struct_matrix_mask.c\
 struct_matvec.c\
 struct_overlap_innerprod.c\
 struct_scale.c\
 struct_stencil.c\
 struct_vector.c

OBJS = ${FILES:.c=.o}

##################################################################
# Targets
##################################################################

all: libHYPRE_struct_mv.a

clean:
	@rm -f *.o *.dxx

veryclean: clean
	@rm -f lib*.a

##################################################################
# Rules
##################################################################

libHYPRE_struct_mv.a: ${OBJS}
	@echo  "Building $@ ... "
	ar -rcu $@ ${OBJS}
	${RANLIB} $@

${OBJS}: ${HEADERS}

##################################################################
# Generic rules
##################################################################
                                                                                               
.c.o:
	${CC} -o $@ -c ${CFLAGS} $<
