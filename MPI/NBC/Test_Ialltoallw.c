
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
	MPI_Datatype * recvtypes = (MPI_Datatype *)malloc(size * sizeof(MPI_Datatype));

	int *send = (int*)malloc(size * 6 * sizeof(int));
	int *sendcounts = (int *)malloc(size * sizeof(int));
	int *sdispls = (int *)malloc(size * sizeof(int));
	MPI_Datatype * sendtypes = (MPI_Datatype *)malloc(size * sizeof(MPI_Datatype));



	for(i=0; i<size ; i++)
	{
		for(j=0; j<6; j++)
		{
			recv[i*6+j] = 0;
			send[i*6+j] = rank * (size*6) + i*6+j;
		}
		recvcounts[i] = 4;
		sendcounts[i] = 4;
		rdispls[i] = i*6 * sizeof(int);
		sdispls[i] = i*5 * sizeof(int);
		recvtypes[i] = MPI_INT;
		sendtypes[i] = MPI_INT;
	}


	MPI_Barrier(MPI_COMM_WORLD);

	MPI_Ialltoallw(send, sendcounts, sdispls, sendtypes, recv, recvcounts, rdispls, recvtypes, MPI_COMM_WORLD, &req);


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
	free(recvtypes);

	free(send);
	free(sendcounts);
	free(sdispls);
	free(sendtypes);

	MPI_Finalize();
	return ret;
}

