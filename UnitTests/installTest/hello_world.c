#include <stdlib.h>
#include <stdio.h>
#ifdef MPC_MODULE_MPC_MPI
#include <mpi.h>
#else
#include <mpc.h>
#endif

int main(int argc,char * argv[])
{
#ifdef MPC_MODULE_MPC_MPI

	int rank, size;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	printf("Nb tasks: %d, Hello world from rank %d!!!\n", size, rank);
	MPI_Finalize();
#else
	printf("Hello world\n");
#endif
	return EXIT_SUCCESS;
}
