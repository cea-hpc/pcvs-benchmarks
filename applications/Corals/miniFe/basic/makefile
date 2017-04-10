#-----------------------------------------------------------------------

MINIFE_TYPES =  \
        -DMINIFE_SCALAR=double   \
        -DMINIFE_LOCAL_ORDINAL=int      \
        -DMINIFE_GLOBAL_ORDINAL=int

MINIFE_MATRIX_TYPE = -DMINIFE_CSR_MATRIX
# MINIFE_MATRIX_TYPE = -DMINIFE_ELL_MATRIX

#-----------------------------------------------------------------------

#kkg CFLAGS = -O3
#kkg CXXFLAGS = -O3

# options for BG/Q
#
CFLAGS = -DIBM_HPM -qlist -qsource  -g -O3 -qsmp=omp
CXXFLAGS = -DIBM_HPM -qlist -qsource -g -O3 -qsmp=omp


# For debugging, the macro MINIFE_DEBUG will cause miniFE to dump a log file
# from each proc containing various information.
# This macro will also enable a somewhat expensive range-check on indices in
# the exchange_externals function.

# CPPFLAGS = -I. -I../utils -I../fem $(MINIFE_TYPES) -DMINIFE_DEBUG -DHAVE_MPI -DMPICH_IGNORE_CXX_SEEK
CPPFLAGS = -I. -I../utils -I../fem $(MINIFE_TYPES) $(MINIFE_MATRIX_TYPE) -DHAVE_MPI -DMPICH_IGNORE_CXX_SEEK

LDFLAGS= -qsmp=omp
LIBS=

# The MPICH_IGNORE_CXX_SEEK macro is required for some mpich versions,
# such as the one on my cygwin machine.

#kkg CXX=mpicxx
#kkg CC=mpicc

# BG/Q
CXX=mpixlcxx_r
CC=mpixlc_r


include make_targets
include ./optional/make_targets

