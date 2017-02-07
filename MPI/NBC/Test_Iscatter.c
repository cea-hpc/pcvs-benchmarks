
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
		MPI_Request req;
	MPI_Init(&argc, &argv);
	
	int i;
	int rank, size;
	int *tab = (int *)malloc(10*sizeof(int));
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	int ret = 0;
	
	int *tab2 = (int*)malloc(size * 10 * sizeof(int));

	for(i=0 ; i<10 ; i++)
		tab[i] = 0;

	for(i=0 ; i<10*size ; i++)
		tab2[i] = i - 10*rank;

	
	MPI_Barrier(MPI_COMM_WORLD);
	
		MPI_Iscatter(tab2, 10, MPI_INT, tab, 10, MPI_INT, 0, MPI_COMM_WORLD, &req);
	
		MPI_Wait(&req, MPI_STATUS_IGNORE);

	MPI_Barrier(MPI_COMM_WORLD);

	for(i=0 ; i<10 ; i++)
	{
		if(tab[i] != 10*rank+i)
		{
			ret = -1;
		}
	}
	
	free(tab);
	free(tab2);
	MPI_Finalize();
	return ret;
}

