#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

double *table;

#pragma hls node(table)

int main( int argc, char **argv )
{
	int rank, size;
	MPI_Status status;
	MPI_Request request;
	int i = 0;
	double *addr;
	
	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );
	
	fprintf(stderr,"addr table = %p\n", &table);

	//node
	if(rank == 0)
	{
		for(i=1 ; i<size ; i++)
		{
			MPI_Recv(&addr, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &status);

			if(addr != (double *)&table)
			{
				fprintf(stderr,"HLS NODE FAILURE\n");
				abort();
			}
		}
		fprintf(stderr,"HLS NODE SUCCESS\n");
	}
	else
	{
		double *pointeur = (double *)&table;
		MPI_Send(&pointeur, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
	}

	MPI_Finalize();
	return EXIT_SUCCESS;
}
