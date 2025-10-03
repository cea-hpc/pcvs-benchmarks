SHELL=/bin/sh
BENCHMARK=lu-mz
BENCHMARKU=LU-MZ

include ../config/make.def

OBJS = lu.o lu_data.o read_input.o setup_omp.o ompnpb.o \
       setcoeff.o setbv.o exact.o setiv.o \
       erhs.o ssor.o zone_setup.o exch_qbc.o \
       rhs.o l2norm.o jacld.o blts.o jacu.o buts.o \
       syncs.o error.o pintgr.o verify.o ${COMMON}/print_results.o \
       ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common


# npbparams.h is included by lu_data module (via lu_data.o)

${PROGRAM}: config
	@if [ x$(VERSION) = xvec -o x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f90.o :
	${FCOMPILE} $<

lu.o:		lu.f90 lu_data.o ompnpb.o
blts.o:		blts.f90
buts.o:		buts.f90
erhs.o:		erhs.f90 lu_data.o
zone_setup.o:   zone_setup.f90 lu_data.o ompnpb.o
exch_qbc.o:     exch_qbc.f90 lu_data.o ompnpb.o
error.o:	error.f90
exact.o:	exact.f90 lu_data.o
jacld.o:	jacld.f90 lu_data.o
jacu.o:		jacu.f90 lu_data.o
l2norm.o:	l2norm.f90
pintgr.o:	pintgr.f90 lu_data.o
read_input.o:	read_input.f90 lu_data.o ompnpb.o
rhs.o:		rhs.f90 lu_data.o
setbv.o:	setbv.f90 lu_data.o
setiv.o:	setiv.f90 lu_data.o
setcoeff.o:	setcoeff.f90 lu_data.o
ssor.o:		ssor.f90 lu_data.o
verify.o:	verify.f90 lu_data.o
setup_omp.o:	setup_omp.f90 lu_data.o ompnpb.o
syncs.o:	syncs.f90
ompnpb.o:	ompnpb.f90  lu_data.o
lu_data.o:	lu_data.f90  npbparams.h

clean:
	- rm -f npbparams.h
	- rm -f *.o *~ *.mod
