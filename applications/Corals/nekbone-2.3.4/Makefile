F77=$(PCVS_FC)
CC=$(PCVS_CC)
NEKBONE_DIR=.

CFLAGS=$(NKFLAGS) $(PCVS_CFLAGS) -g

all:
	mkdir -p obj
	${F77} -c  -O3 $(CFLAGS)  -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/cg.f -o obj/cg.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/driver.f -o obj/driver.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/math.f -o obj/math.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/omp.f -o obj/omp.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/mxm_wrapper.f -o obj/mxm_wrapper.o 
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/prox_dssum.f -o obj/prox_dssum.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/prox_setup.f -o obj/prox_setup.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/semhat.f -o obj/semhat.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/speclib.f -o obj/speclib.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/comm_mpi.f -o obj/comm_mpi.o
	${F77} -c  -O3 $(CFLAGS) -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/mxm_std.f -o obj/mxm_std.o 
	${F77} -c  -O3      -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG -I${NEKBONE_DIR}/test/example1 -I${NEKBONE_DIR}/src -I./ ${NEKBONE_DIR}/src/blas.f -o obj/blas.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/gs.c -o obj/jl_gs.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/sort.c -o obj/jl_sort.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/sarray_transfer.c -o obj/jl_sarray_transfer.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/sarray_sort.c -o obj/jl_sarray_sort.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/gs_local.c -o obj/jl_gs_local.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/crystal.c -o obj/jl_crystal.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/comm.c -o obj/jl_comm.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/tensor.c -o obj/jl_tensor.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/fail.c -o obj/jl_fail.o 
	${CC} -c  -O3 -DTIMERS -DPTRSIZE8 -DMPI -DLONGINT8 -DUNDERSCORE -DGLOBAL_LONG_LONG  -DPREFIX=jl_ ${NEKBONE_DIR}/src/jl/fcrystal.c -o obj/jl_fcrystal.o 
	${F77} $(PCVS_CFLAGS) -o nekbone  obj/cg.o obj/driver.o obj/math.o obj/omp.o obj/mxm_wrapper.o obj/prox_dssum.o obj/prox_setup.o obj/semhat.o obj/speclib.o obj/comm_mpi.o obj/mxm_std.o obj/blas.o obj/jl_gs.o obj/jl_sort.o obj/jl_sarray_transfer.o obj/jl_sarray_sort.o obj/jl_gs_local.o obj/jl_crystal.o obj/jl_comm.o obj/jl_tensor.o obj/jl_fail.o obj/jl_fcrystal.o 
	mv nekbone ./test/example1
