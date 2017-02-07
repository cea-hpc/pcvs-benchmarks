#include <mpi.h>


int main( int argc, char **argv )
{

	MPI_Init( &argc, &argv );

	/* Lets try to create some Infos */

	MPI_Info info[64];

	int i;

	for( i = 0 ; i < 64 ; i++ )
	{
		int ret = MPI_Info_create( &info[i] );

		/* Check success */
		if( ret != MPI_SUCCESS )
		{
			printf("Error creating an MPI_Info\n");
			abort();
		}

		/* Check unique numbering */
		if( (int)(info[i]) != i )
		{
			abort();
		}

	}
	
	MPI_Finalize();

	return 0;
}
