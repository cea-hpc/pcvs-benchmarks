#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>


int main( int argc, char **argv )
{

	MPI_Init( &argc, &argv );

	/* Lets try to free some Infos */

	MPI_Info info;

	MPI_Info_create( &info );

	int ret = MPI_Info_free( &info );
	
	if( ret != MPI_SUCCESS )
	{
		printf("Error deleting an MPI_Info\n");
		abort();
	}

	if( info != MPI_INFO_NULL )
	{
		printf("Error deleted MPI_Info has not been set to MPI_INFO_NULL\n");
		abort();
	}
	
	MPI_Finalize();

	return 0;
}
