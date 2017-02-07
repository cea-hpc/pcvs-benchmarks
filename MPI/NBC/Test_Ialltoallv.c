
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Init(&argc, &argv);
	MPI_Request req;
	
	int i, j;
	int rank, size;
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	

	int *recv = (int*)malloc(size * 6 * sizeof(int));
	int *recvcounts = (int *)malloc(size * sizeof(int));
	int *rdispls = (int *)malloc(size * sizeof(int));

	int *send = (int*)malloc(size * 6 * sizeof(int));
	int *sendcounts = (int *)malloc(size * sizeof(int));
	int *sdispls = (int *)malloc(size * sizeof(int));

	for(i=0; i<size ; i++)
	{
		for(j=0; j<6; j++)
		{
			recv[i*6+j] = 0;
			send[i*6+j] = rank * (size*6) + i*6+j;
		}
		recvcounts[i] = 4;
		sendcounts[i] = 4;
		rdispls[i] = i*6;
		sdispls[i] = i*5;
	}
/*
	if(rank==1)
	{
		for(i=0 ; i<size*6 ; i++)
			fprintf(stderr, "send[%d]=%d\n", i, send[i]);
		fprintf(stderr, "\n");
	}
*/	
	MPI_Barrier(MPI_COMM_WORLD);
	
	MPI_Ialltoallv(send, sendcounts, sdispls, MPI_INT, recv, recvcounts, rdispls, MPI_INT, MPI_COMM_WORLD, &req);
	
	MPI_Wait(&req, MPI_STATUS_IGNORE);
	
	MPI_Barrier(MPI_COMM_WORLD);

	for(i=0 ; i<size * 6 ; i++)
	{
		if(i%6 != 4 && i%6 != 5)
		{
			if(recv[i] != (rank*5+i%6) + (i/6)*(size*6)) ret = -1;
		}
		else
		{
			if(recv[i] != 0) ret = -1;
		}
	}
	
	
	free(recv);
	free(recvcounts);
	free(rdispls);

	free(send);
	free(sendcounts);
	free(sdispls);

	MPI_Finalize();
	return ret;
}

