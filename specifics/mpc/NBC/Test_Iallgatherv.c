
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#ifdef NBC
#include <nbc.h>
#endif
int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Request req;
	MPI_Init(&argc, &argv);
	
	int i, j;
	int rank, size;
	int *tab = (int *)malloc(4*sizeof(int));
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	
	int *recv = (int*)malloc(size * 6 * sizeof(int));
	int *recvcounts = (int *)malloc(size * sizeof(int));
	int * displs = (int *)malloc(size * sizeof(int));

	for(i=0; i<size ; i++)
	{
		for(j=0; j<6; j++)
		recv[i*6+j] = 0;
		recvcounts[i] = 4;
		displs[i] = i*6;
	}

	for(i=0 ; i<4 ; i++)
		tab[i] = rank * 4 + i;
	
	MPI_Barrier(MPI_COMM_WORLD);
	
	MPI_Iallgatherv(tab, 4, MPI_INT, recv, recvcounts, displs, MPI_INT, MPI_COMM_WORLD, &req);
	
	MPI_Wait(&req, MPI_STATUS_IGNORE);
	
	MPI_Barrier(MPI_COMM_WORLD);
	
	int cpt = -1;
	for(i=0 ; i<size *6 ; i++)
	{
		if( i%6 != 4 && i%6 != 5 )
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
	free(tab);
	free(recv);
	free(recvcounts);
	free(displs);
	MPI_Finalize();

	

	return ret;
}

