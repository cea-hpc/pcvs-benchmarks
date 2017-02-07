CC=${TS_CC}

.PHONY: all
all:
	@echo "Current valid targets:"
	@echo "${MAKE} icc: Uses Intel's x86 linux compiler"
	@echo "${MAKE} bgq: Uses IBM's BGQ compiler"
	@echo "${MAKE} gcc: Uses Gnu's x86 linux compiler"


.PHONY: icc
icc: clomp.c
	icc -openmp -O3 clomp.c -o clomp
	@echo "=> Compile of clomp completed."
	@echo " "
	mpiicc -DWITH_MPI -openmp -O3 clomp.c -o clomp_mpi
	@echo "=> Compile of clomp_mpi completed."

.PHONY: bgq
bgq: clomp.c
	@echo "=> Starting compile of clomp"
	bgxlc_r -qsmp=omp -qthreaded -O3 clomp.c -o clomp -lm
	#/usr/local/tools/compilers/ibm/mpixlc_r-lompbeta2 -qsmp=omp -qthreaded -O3 clomp.c -o clomp -lm
	@echo "=> Compile of clomp completed."
	@echo " "
	@echo "=> Starting compile of clomp_mpi"
	mpixlc_r -DWITH_MPI -qsmp=omp -qthreaded -O3 clomp.c -o clomp_mpi -lm
	#/usr/local/tools/compilers/ibm/mpixlc_r-lompbeta2  -DWITH_MPI -qsmp=omp -qthreaded -O3 clomp.c -o clomp_mpi -lm
	@echo "=> Compile of clomp_mpi completed."

.PHONY: gcc
gcc: clomp.c
	@echo "=> Starting compile of clomp"
	gcc --openmp -O3 clomp.c -o clomp -lm
	@echo "=> Compile of clomp completed."
	@echo " "
	@echo "=> Starting compile of clomp_mpi"
	mpigcc -DWITH_MPI --openmp -O3 clomp.c -o clomp_mpi -lm
	@echo "=> Compile of clomp_mpi completed."

.PHONY: mpc
mpc: clomp.c
	@echo "=> Starting compile of clomp with mpc"
	${CC} -fopenmp ${PRIVATIZATION} -O3 clomp.c -o clomp -lm
	@echo "=> Compile of clomp completed."
	@echo " "
	@echo "=> Starting compile of clomp_mpi"
	${CC} -DWITH_MPI ${PRIVATIZATION} -fopenmp -O3 clomp.c -o clomp_mpi -lm
	@echo "=> Compile of clomp_mpi completed."

clean: 
	rm clomp
	rm clomp_mpi
	rm *.out
	rm run_clomp.summary
	rm run_clomp.CORAL_RFP
