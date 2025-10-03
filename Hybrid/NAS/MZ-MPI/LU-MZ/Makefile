SHELL=/bin/sh
BENCHMARK=lu-mz
BENCHMARKU=LU-MZ
F08=def

include ../config/make.def

OBJS = lu.o lu_data.o read_input.o setup_mpi.o \
       setcoeff.o setbv.o exact.o setiv.o \
       erhs.o ssor.o zone_setup.o exch_qbc.o \
       rhs.o l2norm.o jacld.o blts.o jacu.o buts.o \
       syncs.o error.o pintgr.o verify.o proc_grid.o neighbors.o \
       subdomain.o exchange_1.o exchange_3.o  exchange_4.o \
       error_cond.o mpinpb.o alloc_space.o \
       ${COMMON}/print_results.o ${COMMON}/timers.o

include ../sys/make.common


# npbparams.h is included by lu_data module (via lu_data.o)

${PROGRAM}: config $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f90.o :
	${FCOMPILE} $<

lu.o:		lu.f90 lu_data.o mpinpb.o
blts.o:		blts.f90
buts.o:		buts.f90
erhs.o:		erhs.f90 lu_data.o
zone_setup.o:   zone_setup.f90 lu_data.o mpinpb.o
exch_qbc.o:     exch_qbc.f90 lu_data.o mpinpb.o
error.o:	error.f90 lu_data.o mpinpb.o
exact.o:	exact.f90 lu_data.o
jacld.o:	jacld.f90 lu_data.o
jacu.o:		jacu.f90 lu_data.o
l2norm.o:	l2norm.f90 lu_data.o mpinpb.o
pintgr.o:	pintgr.f90 lu_data.o mpinpb.o
read_input.o:	read_input.f90 lu_data.o mpinpb.o
rhs.o:		rhs.f90 lu_data.o
setbv.o:	setbv.f90 lu_data.o
setiv.o:	setiv.f90 lu_data.o
setcoeff.o:	setcoeff.f90 lu_data.o
ssor.o:		ssor.f90 lu_data.o mpinpb.o
verify.o:	verify.f90 lu_data.o
setup_mpi.o:	setup_mpi.f90 lu_data.o mpinpb.o
proc_grid.o:	proc_grid.f90 lu_data.o mpinpb.o
neighbors.o:	neighbors.f90 lu_data.o mpinpb.o
subdomain.o:	subdomain.f90 lu_data.o mpinpb.o
exchange_1.o:	exchange_1.f90 lu_data.o mpinpb.o
exchange_3.o:	exchange_3.f90 lu_data.o mpinpb.o
exchange_4.o:	exchange_4.f90 lu_data.o mpinpb.o
error_cond.o:   error_cond.f90  mpinpb.o
syncs.o:	syncs.f90 lu_data.o
alloc_space.o:  alloc_space.f90 lu_data.o mpinpb.o
lu_data.o:      lu_data.f90 npbparams.h
mpinpb.o:       mpinpb.f90 lu_data.o
mpinpb.f90:     mpinpb_${F08}.f90
	cp mpinpb_${F08}.f90 $@

clean:
	- rm -f *.o *~ *.mod mputil* core
	- rm -f npbparams.h mpinpb.f90
