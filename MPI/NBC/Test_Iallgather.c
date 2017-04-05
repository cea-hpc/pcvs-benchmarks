
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	MPI_Request req;
	MPI_Init(&argc, &argv);
	
	int ret = 0;
	int i;
	int rank, size;
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	
	
	int count = 0;
	
	for(count=1; count <= 65536; count *=2)
	{
		int *recv = (int *)malloc(size * count * sizeof(int));
		int *tab = ( int *)malloc(       count * sizeof(int));

		for(i=0 ; i<count ; i++)
			tab[i] = rank * count + i;


		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Iallgather(tab, count, MPI_INT, recv, count, MPI_INT, MPI_COMM_WORLD, &req);

		MPI_Wait(&req, MPI_STATUS_IGNORE);

		MPI_Barrier(MPI_COMM_WORLD);



		for(i=0 ; i<size * count ; i++)
			if(recv[i] != i) ret = -1;

		free(tab);
		free(recv);
	}
	MPI_Finalize();
	return ret;
}

