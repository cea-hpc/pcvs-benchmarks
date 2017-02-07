#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

double *privatize;

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

		
	//privatization
	for(i=0 ; i<size ; i++)
	{
		if(i != rank)
		{
			double *pointeur = (double *)&privatize;
			MPI_Isend(&pointeur, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &request);
			MPI_Irecv(&addr, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &request);

			if(addr == (double *)&privatize)
			{
				fprintf(stderr,"PRIVATIZATION FAILURE\n");
				abort();
			}
		}
	}
	MPI_Wait(&request, &status);
	MPI_Barrier(MPI_COMM_WORLD);
	if(rank == 0)
		fprintf(stderr,"PRIVATIZATION SUCCESS\n");
	MPI_Finalize();
	return EXIT_SUCCESS;
}
