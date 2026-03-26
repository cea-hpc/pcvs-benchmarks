#-----------------------------------------------------------------------
# This file compiles for OpenMP and MPI hybrid operations using the GNU
# compile chain.

MINIFE_TYPES =  \
        -DMINIFE_SCALAR=double \
        -DMINIFE_LOCAL_ORDINAL=int      \
        -DMINIFE_GLOBAL_ORDINAL=int

MINIFE_MATRIX_TYPE = -DMINIFE_CSR_MATRIX
#MINIFE_MATRIX_TYPE = -DMINIFE_ELL_MATRIX

#-----------------------------------------------------------------------

CFLAGS = -O3 -xMIC-AVX512 -g -std=c++11 $(PCVS_CFLAGS)

CXXFLAGS = $(CFLAGS)

CPPFLAGS = -I. -I../utils -I../fem $(MINIFE_TYPES) \
	$(MINIFE_MATRIX_TYPE) \
	-DMINIFE_RESTRICT=__restrict__ \
	-DHAVE_MPI -DMPICH_IGNORE_CXX_SEEK

LDFLAGS=$(CFLAGS)
LIBS=-lmemkind -lnuma

# The MPICH_IGNORE_CXX_SEEK macro is required for some mpich versions,
# such as the one on my cygwin machine.

CXX=$(PCVS_CXX)
CC=$(PCVS_CC)

include make_targets
