
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	MPI_Init(&argc, &argv);
	MPI_Request req;
	
	int i;
	int rank, size;
	int ret = 0;
	int count = 0;
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	for(count=1; count <= 65536; count *=2)
	{

		int *tab = (int *)malloc(count*sizeof(int));	
		int *recv = (int*)malloc(size * count * sizeof(int));

		for(i=0 ; i<count ; i++)
			tab[i] = rank * count + i;


		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Igather(tab, count, MPI_INT, recv, count, MPI_INT, 0, MPI_COMM_WORLD, &req);


		MPI_Wait(&req, MPI_STATUS_IGNORE);

		MPI_Barrier(MPI_COMM_WORLD);

		if(rank == 0)
		{	
			for(i=0 ; i<size *count ; i++)
			{
				if(recv[i] != i) ret = -1;
			}
		}

		free(tab);
		free(recv);
	}
	MPI_Finalize();
	return ret;
}

