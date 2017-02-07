#include <mpi.h>
#include <string.h>

int main( int argc, char **argv )
{

	MPI_Init( &argc, &argv );

	/* Lets try to set and get some Infos */

	MPI_Info info;

	MPI_Info_create( &info );


	/* SET ========= */
	char * expected_keys[3] = {"KEY1", "KEY2", "KEY3" };
	
	MPI_Info_set( info, expected_keys[0], "This is not finished !");
	MPI_Info_set( info, expected_keys[1], "This is not finished ! 1");
	MPI_Info_set( info, expected_keys[2], "This is not finished ! 2");

	int count = 0;

	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );
	
	/* GETNTH ========= */
	
	int i;

	for( i = 0 ; i < count ; i++ )
	{
		char key[MPI_MAX_INFO_KEY];
		MPI_Info_get_nthkey( info , i, key );
		printf("%d => %s \n", i, key );

		if( strcmp( key, expected_keys[i]))
			abort();
	}		
	
	MPI_Info_free( &info );
		
	MPI_Finalize();

	return 0;
}
