
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
		MPI_Request req;
	MPI_Init(&argc, &argv);
	
	int i, j;
	int rank, size;
	int ret = 0;

	int *tab = (int *)malloc(10*sizeof(int));
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	
	int *send = (int*)malloc(size * 12 * sizeof(int));
	int *sendcounts = (int *)malloc(size * sizeof(int));
	int * displs = (int *)malloc(size * sizeof(int));
	for(i=0; i<size ; i++)
	{
		for(j=0; j<12; j++)
		{
			send[i*12+j] = i*12+j;
		}
		sendcounts[i] = 10;
		displs[i] = i*12;
	}

	for(i=0 ; i<10 ; i++)
		tab[i] = 0;
	
	
	MPI_Barrier(MPI_COMM_WORLD);
	
		MPI_Iscatterv(send, sendcounts, displs, MPI_INT, tab, 10, MPI_INT, 0, MPI_COMM_WORLD, &req);
	
       	
		MPI_Wait(&req, MPI_STATUS_IGNORE);
	
	MPI_Barrier(MPI_COMM_WORLD);

	for(i=0 ; i<10 ; i++)
	{
		if(tab[i] != rank*12+i)
		{
			ret = -1;
		}
	}
	
	free(tab);
	free(send);
	free(sendcounts);
	free(displs);
	MPI_Finalize();
	return ret;
}

