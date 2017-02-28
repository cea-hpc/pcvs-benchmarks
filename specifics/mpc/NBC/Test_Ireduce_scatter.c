
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#ifdef NBC
#include <nbc.h>
#endif
int main(int argc, char **argv)
{
	MPI_Init(&argc, &argv);

	MPI_Request req;

	int i;
	int rank, size;

	int val, red, sum;

	red=0;
	sum=0;
	val=0;
	int ret = 0;

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	val = rank;
	int *send = (int*)malloc(size * sizeof(int));
	int *recv = (int*)malloc(size * sizeof(int));
	int *count = (int*)malloc(size * sizeof(int));


	for(i=0 ; i<size ; i++)
	{
		send[i] = rank * size + i;
		recv[i] = 0;
		count[i] = 1;
	}


	MPI_Barrier(MPI_COMM_WORLD);

	MPI_Ireduce_scatter(send, recv, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req);


	MPI_Wait(&req, MPI_STATUS_IGNORE);

	for(i=0; i< size; i++) sum +=(size)*i+rank;


	MPI_Barrier(MPI_COMM_WORLD);



	if(recv[0] != sum)
	{
		ret = -1;
	}

	MPI_Finalize();
	return ret;
}

