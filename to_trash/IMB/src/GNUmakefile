default: MPI1

all: EXT IO MPI1

MPI1: feedback
	gmake -f make_ict IMB-MPI1

EXT:
	gmake -f make_ict IMB-EXT

IO:
	gmake -f make_ict IMB-IO

feedback:
	@echo " "
	@echo "This Makefile uses make_ict to build IMB "
	@echo "based on \"ICT\" (Intel(R) Cluster Toolkit)"
	@echo " "
	@echo "Targets: MPI1 (default), EXT, IO, all, clean"
	@echo "         MPI1 builds the MPI-1 standard benchmarks"
	@echo "         EXT builds the one sided communications benchmarks"
	@echo "         IO builds the MPI-File I\/O benchmarks"
	@echo "         all = MPI1+EXT+IO"
	@echo " "
	@echo "When an Intel(r) MPI Library install and mpiicc path exists,"
	@echo "this should work immediately."
	@echo " "
	@echo "Alternatively, use "
	@echo " "
	@echo "gmake -f make_mpich"
	@echo " "
	@echo "to install an mpich or similar version; for this,"
	@echo "you normally have to edit at least the MPI_HOME"
	@echo "variable provided in make_mpich"
	@echo " "

clean:
	/bin/rm -f *.o *~ PI* core IMB-IO IMB-EXT IMB-MPI1 exe_io exe_ext exe_mpi1 IMB_out IMB_out_*
