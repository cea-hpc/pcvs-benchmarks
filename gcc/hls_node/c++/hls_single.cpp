#include <mpi.h>
#include <cstdlib>
#include <iostream>

using namespace std;

int compteur_node = 0;

#pragma hls node(compteur_node)

int main( int argc, char **argv )
{
	int rank, size;
	MPI_Status status;
	MPI_Request request;
	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

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
			cout << "HLS NODE SINGLE FAILURE\n" << endl;
			abort();
		}
		else
		{
			cout << "HLS NODE SINGLE SUCCESS\n" << endl;
		}
	}
	
	MPI_Finalize();
	return EXIT_SUCCESS;
}
