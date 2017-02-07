
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
	int send;
	int recv;
 	int count;

	int val, red, sum;

	red=0;
	sum=0;
	val=0;
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	count = size;
 	send = rank;
	recv = 0;
	
	MPI_Barrier(MPI_COMM_WORLD);
	
	MPI_Iscan(&send, &recv, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &req);
	
	
      	MPI_Wait(&req, MPI_STATUS_IGNORE);
	for(i=0; i<=rank; i++) sum +=i;

	MPI_Barrier(MPI_COMM_WORLD);

	if(recv != sum) ret = -1;	
	
	MPI_Finalize();
	return ret;
}

