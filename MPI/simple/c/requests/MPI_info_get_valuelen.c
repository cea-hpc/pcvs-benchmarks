#include <stdio.h>
#include <stdlib.h>
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
	char * expected_values[3] = {"123", "12345", "1234567890" };
	
	MPI_Info_set( info, expected_keys[0], expected_values[0]);
	MPI_Info_set( info, expected_keys[1], expected_values[1]);
	MPI_Info_set( info, expected_keys[2], expected_values[2]);

	int count = 0;

	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );
	
	/* GETNTH ========= */
	
	int i;
	int valuelen = 0;
	int flag = 0;
		
	/* Test existing */
	for( i = 0 ; i < count ; i++ )
	{
		MPI_Info_get_valuelen(info, expected_keys[i], &valuelen, &flag);
		
		if( flag != 1 )
			abort();
		
		if( strlen( expected_values[i] ) != valuelen )
		{
			abort();
		}

	}		
	
	/* Test not existing */
		
	MPI_Info_get_valuelen(info, "a_strange_key", &valuelen, &flag);
	
	if( flag != 0 )
		abort();
	
	
	MPI_Info_free( &info );
		
	MPI_Finalize();

	return 0;
}
