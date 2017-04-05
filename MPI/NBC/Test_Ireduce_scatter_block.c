
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#ifdef NBC
#include <nbc.h>
#endif

#define RANK 1

int main(int argc, char **argv)
{
	MPI_Init(&argc, &argv);

	MPI_Request req;

	int i, j;
	int rank, size;

	int val, red, sum;

	red=0;
	sum=0;
	val=0;

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	val = rank;
	int *send = (int*)malloc(size * 4 * sizeof(int));
	int *recv = (int*)malloc(size * 4 *sizeof(int));

	int ret = 0;

	for(i=0 ; i<size ; i++)
	{
		for(j=0 ; j<4 ; j++)
		{
			send[i*4+j] = rank * size + i *4 +j;
			recv[i*4+j] = 0;
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);

	MPI_Ireduce_scatter_block(send, recv, 4, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req);


	MPI_Wait(&req, MPI_STATUS_IGNORE);


	MPI_Barrier(MPI_COMM_WORLD);
	for(j=0; j<4; j++)
	{
		sum = 0;
		for(i=0; i< size; i++) 
		{
			sum +=(size)*i+j+rank*4;
		}

		if(recv[j] != sum)
		{
			ret = -1;
		}
	}


	MPI_Finalize();
	return ret;
}

