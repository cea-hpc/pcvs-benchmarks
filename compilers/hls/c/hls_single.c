#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int compteur_node = 0;
int temp = 0;

#pragma hls node(temp)
#pragma hls node(compteur_node)

int main( int argc, char **argv )
{
	int rank, size;
	MPI_Status status;
	MPI_Request request;

	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );
	
	fprintf(stderr,"addr compteur_node %p\n", &compteur_node);
		
	#pragma hls barrier(compteur_node)
	
	#pragma hls single(compteur_node)
	{
		compteur_node++;
	}
	
	#pragma hls barrier(compteur_node)
	
	if(rank == 0)
	{
		//node
		if(compteur_node != 1)
		{
			fprintf(stderr,"HLS NODE SINGLE FAILURE\n");
			abort();
		}
		else
		{
			fprintf(stderr,"HLS NODE SINGLE SUCCESS\n");
		}
	}

	MPI_Finalize();
	return EXIT_SUCCESS;
}
