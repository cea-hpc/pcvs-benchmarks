AUTOMAKE_OPTIONS = subdir-objects

NVCC = nvcc
NVCFLAGS = -cuda -maxrregcount 32 -ccbin $(CXX)
SUFFIXES = .cu .cpp
.cu.cpp:
	$(NVCC) $(NVCFLAGS) $(INCLUDES) $(CPPFLAGS) --output-file $@ $<

collectivedir = $(pkglibexecdir)/mpi/collective
collective_PROGRAMS = osu_alltoallv osu_allgatherv osu_scatterv osu_gatherv osu_reduce_scatter osu_barrier osu_reduce osu_allreduce osu_alltoall osu_bcast osu_gather osu_allgather osu_scatter osu_iallgather osu_ibcast  osu_ialltoall osu_ibarrier osu_igather osu_iscatter osu_iscatterv osu_igatherv osu_iallgatherv osu_ialltoallv osu_ialltoallw

AM_CFLAGS = -I../../util

UTILITIES = ../../util/osu_util.c osu_util.h
if CUDA_KERNELS
UTILITIES += ../../util/kernel.cu
endif

osu_allgatherv_SOURCES = osu_allgatherv.c $(UTILITIES)
osu_scatterv_SOURCES = osu_scatterv.c $(UTILITIES)
osu_gather_SOURCES = osu_gather.c $(UTILITIES)
osu_gatherv_SOURCES = osu_gatherv.c $(UTILITIES)
osu_reduce_scatter_SOURCES = osu_reduce_scatter.c $(UTILITIES)
osu_barrier_SOURCES = osu_barrier.c $(UTILITIES)
osu_reduce_SOURCES = osu_reduce.c $(UTILITIES)
osu_allreduce_SOURCES = osu_allreduce.c $(UTILITIES)
osu_bcast_SOURCES = osu_bcast.c $(UTILITIES)
osu_alltoall_SOURCES = osu_alltoall.c $(UTILITIES)
osu_alltoallv_SOURCES = osu_alltoallv.c $(UTILITIES)
osu_ialltoall_SOURCES = osu_ialltoall.c $(UTILITIES)
osu_ialltoallv_SOURCES = osu_ialltoallv.c $(UTILITIES)
osu_ialltoallw_SOURCES = osu_ialltoallw.c $(UTILITIES)
osu_ibarrier_SOURCES = osu_ibarrier.c $(UTILITIES)
osu_ibcast_SOURCES = osu_ibcast.c $(UTILITIES)
osu_igather_SOURCES = osu_igather.c $(UTILITIES)
osu_igatherv_SOURCES = osu_igatherv.c $(UTILITIES)
osu_allgather_SOURCES = osu_allgather.c $(UTILITIES)
osu_iallgather_SOURCES = osu_iallgather.c $(UTILITIES)
osu_iallgatherv_SOURCES = osu_iallgatherv.c $(UTILITIES)
osu_scatter_SOURCES = osu_scatter.c $(UTILITIES)
osu_iscatter_SOURCES = osu_iscatter.c $(UTILITIES)
osu_iscatterv_SOURCES = osu_iscatterv.c $(UTILITIES)

if EMBEDDED_BUILD
    AM_LDFLAGS =
    AM_CPPFLAGS = -I$(top_builddir)/../src/include \
          -I${top_srcdir}/util \
		  -I${top_srcdir}/../src/include
if BUILD_PROFILING_LIB
    AM_LDFLAGS += $(top_builddir)/../lib/lib@PMPILIBNAME@.la
endif
    AM_LDFLAGS += $(top_builddir)/../lib/lib@MPILIBNAME@.la
endif

if OPENACC
    AM_CFLAGS += -acc
    AM_CXXFLAGS = -acc
endif
