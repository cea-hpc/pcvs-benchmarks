
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	MPI_Request req;
	MPI_Init(&argc, &argv);

	int i, j;
	int rank, size;

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	int ret = 0;

	int *recv = (int*)malloc(size * 12 * sizeof(int));
	int *recvcounts = (int *)malloc(size * sizeof(int));
	int * displs = (int *)malloc(size * sizeof(int));
	int *tab = (int *)malloc(10*sizeof(int));


	for(i=0; i<size ; i++)
	{
		for(j=0; j<12; j++)
			recv[i*12+j] = 0;
		recvcounts[i] = 10;
		displs[i] = i*12;
	}

	for(i=0 ; i<10 ; i++)
		tab[i] = rank*10+i;


	MPI_Barrier(MPI_COMM_WORLD);

	MPI_Igatherv( tab, 10, MPI_INT, recv, recvcounts, displs, MPI_INT, 0, MPI_COMM_WORLD, &req);


	MPI_Wait(&req, MPI_STATUS_IGNORE);

	MPI_Barrier(MPI_COMM_WORLD);

	if(rank == 0)
	{
		int cpt = -1;
		for(i=0 ; i<size *12 ; i++)
		{
			if( i%12 != 10 && i%12 != 11 )
			{	
				cpt++;
				if(recv[i] != cpt)
				{ 
					ret = -1;
				}
			}
			else
			{
				if(recv[i] != 0)
				{ 
					ret = -1;
				}
			}
		}
	}

	free(tab);
	free(recv);
	free(recvcounts);
	free(displs);
	MPI_Finalize();
	return ret;
}

