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
# $Revision: 1.4 $
#EHEADER**********************************************************************

.SUFFIXES:
.SUFFIXES: .c .f .o
                                                                                               
srcdir = .
                                                                                               
include ../Makefile.include
                                                                                               
CFLAGS =\
 -I..\
 -I../utilities\
 -I../krylov\
 -I../seq_mv\
 -I../parcsr_mv\
 -I../IJ_mv\
 ${INCLUDE_CFLAGS}
RANLIB = ranlib
 
HEADERS =\
 aux_interp.h\
 headers.h\
 HYPRE_parcsr_ls.h\
 par_amg.h\
 parcsr_ls.h

FILES =\
 aux_interp.c\
 gen_redcs_mat.c\
 HYPRE_parcsr_amg.c\
 HYPRE_parcsr_gmres.c\
 HYPRE_parcsr_pcg.c\
 par_amg.c\
 par_amg_setup.c\
 par_amg_solve.c\
 par_cg_relax_wt.c\
 par_coarsen.c\
 par_coarse_parms.c\
 par_cycle.c\
 par_indepset.c\
 par_interp.c\
 par_jacobi_interp.c\
 par_multi_interp.c\
 par_laplace_27pt.c\
 par_laplace.c\
 par_lr_interp.c\
 par_nodal_systems.c\
 par_rap.c\
 par_rap_communication.c\
 par_vardifconv.c\
 par_relax.c\
 par_relax_interface.c\
 par_scaled_matnorm.c\
 par_stats.c\
 par_strength.c\
 partial.c\
 pcg_par.c\
 par_relax_more.c
OBJS = ${FILES:.c=.o}

##################################################################
# Targets
##################################################################

all: libparcsr_ls.a
 
clean:
	@rm -f *.o *.dxx
                                                                                               
veryclean: clean
	@rm -f lib*.a

##################################################################
# Rules
##################################################################

libparcsr_ls.a: ${OBJS}
	@echo  "Building $@ ... "
	ar -rcu $@ ${OBJS}
	${RANLIB} $@

${OBJS}: ${HEADERS}

##################################################################
# Generic rules
##################################################################
                                                                                               
.c.o:
	${CC} -o $@ -c ${CFLAGS} $<
