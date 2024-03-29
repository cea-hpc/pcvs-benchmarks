
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#ifdef NBC
#include <nbc.h>
#endif
int main(int argc, char **argv)
{
	int ret = 0;
	MPI_Init(&argc, &argv);

	MPI_Request req1, req2;
	
	int i;
	int rank, size;

	int val, red, sum;
	int *send, *recv;

	int count = 0;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	for(count = 1; count <= 65536; count *=2)
	{

		send=malloc(sizeof(int)*count);
		recv=malloc(sizeof(int)*count);

		red=0;
		sum=0;
		val=0;


		val = rank;
		for(i=0; i<count; i++) send[i] = rank;


		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Iallreduce(&val, &red, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req1);
		MPI_Iallreduce(send, recv, count, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req2);

		MPI_Wait(&req1, MPI_STATUS_IGNORE);
		MPI_Wait(&req2, MPI_STATUS_IGNORE);

		for(i=0; i< size; i++) sum +=i;

		MPI_Barrier(MPI_COMM_WORLD);

		if(red != sum) ret = -1;	
		for(i=0; i<count; i++)
		{
			if(recv[i] != sum)
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

