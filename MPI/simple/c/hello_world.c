#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc,char * argv[])
{
	int rank, size;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	printf("Nb tasks: %d, Hello world from rank %d!!!\n", size, rank);
	MPI_Finalize();
	printf("Hello world\n");
	return EXIT_SUCCESS;
}
