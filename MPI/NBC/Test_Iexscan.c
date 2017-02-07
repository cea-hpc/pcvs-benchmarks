
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Init(&argc, &argv);

	MPI_Request req;
	
	int i;
	int rank, size;
	int *send, *recv;
 	int count = 0;

	int val, red, sum;

	red=0;
	sum=0;
	val=0;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	for(i=0; i<rank; i++) sum +=i;

	for(count = 1; count <= 65536; count *= 2)
	{
		send = malloc(sizeof(int)*count);
		recv = malloc(sizeof(int)*count);

		for(i=0; i<count; i++)
		{
			send[i] = rank;
			recv[i] = 0;
		}

		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Iexscan(send, recv, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req);


		MPI_Wait(&req, MPI_STATUS_IGNORE);


		MPI_Barrier(MPI_COMM_WORLD);

		for(i=0; i<count; i++)
		{
			if(recv[i] != sum && rank != 0)
			{
				ret = -1;	
			}
		}
		free(send);
		free(recv);
	}

	MPI_Finalize();
	return ret;
}

