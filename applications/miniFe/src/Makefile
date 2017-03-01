#-----------------------------------------------------------------------
# This file compiles for OpenMP and MPI hybrid operations using the GNU
# compile chain.

MINIFE_TYPES =  \
        -DMINIFE_SCALAR=double   \
        -DMINIFE_LOCAL_ORDINAL=int      \
        -DMINIFE_GLOBAL_ORDINAL="long long int"

MINIFE_MATRIX_TYPE = -DMINIFE_CSR_MATRIX
#MINIFE_MATRIX_TYPE = -DMINIFE_ELL_MATRIX

#-----------------------------------------------------------------------

# GNU CFLAGS = -O3 -fopenmp

# BG/Q
#CFLAGS = -DIBM_HPM -qlist -qsource  -g -O3 -qsmp=omp
CFLAGS = -qlist -qsource  -g -O3 -qsmp=omp
CXXFLAGS = $(CFLAGS)

# To enable memory high water mark reporting uncomment the
# report rusage
CPPFLAGS = -I. -I../utils -I../fem $(MINIFE_TYPES) \
	$(MINIFE_MATRIX_TYPE) \
	-DHAVE_MPI -DMPICH_IGNORE_CXX_SEEK #-DMINIFE_REPORT_RUSAGE

LDFLAGS=$(CFLAGS)
LIBS=

# GNU CXX=mpicxx
# GNU CC=mpicc

# BG/Q
CXX=mpixlcxx_r
CC=mpixlc_r


include make_targets
