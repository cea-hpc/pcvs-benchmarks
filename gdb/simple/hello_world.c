#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifdef MPC_MODULE_MPC_MPI
#include <mpi.h>
#else
#include <mpc.h>
#endif

int main(int argc,char * argv[])
{
char* toto = NULL;
 static char NAME[4096];

gethostname(NAME,4096);


#ifdef MPC_MODULE_MPC_MPI

	int rank, size;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Barrier(MPI_COMM_WORLD);

	printf("Nb tasks: %d, Hello world from rank %d %s!!!\n", size, rank,NAME);
#else
	printf("Hello world %s\n",NAME);
#endif
#ifdef MPC_MODULE_MPC_MPI

	MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();
#endif

	return EXIT_SUCCESS;
}
