
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>


int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Init(&argc, &argv);

	MPI_Request req[10];

	int i, j;
	int rank, size;

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	for(i=0; i< 10; i++)
	{
		MPI_Barrier(MPI_COMM_WORLD);


		for(j=0; j<i; j++)
		{
			MPI_Ibarrier(MPI_COMM_WORLD, &(req[j]));
		}

		for(j=0; j<i; j++)
		{
			MPI_Wait(&(req[j]), MPI_STATUS_IGNORE);
		}
			MPI_Barrier(MPI_COMM_WORLD);

	}
	MPI_Finalize();
	return ret;
}

