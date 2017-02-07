
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Request req;
	MPI_Init(&argc, &argv);
	
	int i;
	int rank, size;
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	
	int root = 7%size;

	int count = 0;
	for(count=1; count <= 65536; count *=2)
	{

		int *tab = (int *)malloc(count*sizeof(int));
		if(rank == root)
		{
			for(i=0 ; i<count ; i++)
				tab[i] = i;
		}
		else
		{
			for(i=0 ; i<count ; i++)
				tab[i] = -1;
		}


		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Ibcast(tab, count, MPI_INT, root, MPI_COMM_WORLD, &req);


		MPI_Wait(&req, MPI_STATUS_IGNORE);

		MPI_Barrier(MPI_COMM_WORLD);

		for(i=0 ; i<count ; i++)
		{
			if(tab[i] != i)
			{
				ret = -1;
			}
		}

		free(tab);
	}
	MPI_Finalize();
	return ret;
}

