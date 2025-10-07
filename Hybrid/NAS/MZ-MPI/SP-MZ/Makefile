SHELL=/bin/sh
BENCHMARK=sp-mz
BENCHMARKU=SP-MZ
F08=def

include ../config/make.def


OBJS = sp.o sp_data.o initialize.o exact_solution.o exact_rhs.o \
       set_constants.o adi.o rhs.o zone_setup.o \
       x_solve.o ninvr.o y_solve.o pinvr.o exch_qbc.o   \
       z_solve.o tzetar.o add.o txinvr.o error.o verify.o \
       setup_mpi.o mpinpb.o error_cond.o alloc_space.o  \
       ${COMMON}/print_results.o ${COMMON}/timers.o

include ../sys/make.common

# npbparams.h is included by sp_data module (via sp_data.o)

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f90.o:
	${FCOMPILE} $<

sp.o:             sp.f90  sp_data.o mpinpb.o
initialize.o:     initialize.f90  sp_data.o
exact_solution.o: exact_solution.f90  sp_data.o
exact_rhs.o:      exact_rhs.f90  sp_data.o
set_constants.o:  set_constants.f90  sp_data.o
adi.o:            adi.f90  sp_data.o
rhs.o:            rhs.f90  sp_data.o
zone_setup.o:     zone_setup.f90  sp_data.o mpinpb.o
x_solve.o:        x_solve.f90  sp_data.o
ninvr.o:          ninvr.f90  sp_data.o
y_solve.o:        y_solve.f90  sp_data.o
pinvr.o:          pinvr.f90  sp_data.o
z_solve.o:        z_solve.f90  sp_data.o
tzetar.o:         tzetar.f90  sp_data.o
add.o:            add.f90  sp_data.o
txinvr.o:         txinvr.f90  sp_data.o
error.o:          error.f90  sp_data.o
verify.o:         verify.f90  sp_data.o mpinpb.o
exch_qbc.o:       exch_qbc.f90  sp_data.o mpinpb.o
setup_mpi.o:      setup_mpi.f90  sp_data.o mpinpb.o
error_cond.o:     error_cond.f90  mpinpb.o
alloc_space.o:    alloc_space.f90  sp_data.o mpinpb.o
sp_data.o:        sp_data.f90  npbparams.h
mpinpb.o:         mpinpb.f90  sp_data.o
mpinpb.f90:       mpinpb_${F08}.f90
	cp mpinpb_${F08}.f90 mpinpb.f90

clean:
	- rm -f *.o *~ *.mod mputil* core
	- rm -f npbparams.h mpinpb.f90
