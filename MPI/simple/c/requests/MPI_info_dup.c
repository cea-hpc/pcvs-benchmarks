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


	/* DUP ========= */
	MPI_Info dupinfo;
	MPI_Info_dup( info, &dupinfo );


	int count = 0;
	
	/* CHECK CONTENT DUP ========= */

	MPI_Info_get_nkeys( dupinfo , &count );

	printf("DUP Count : %d\n", count );
	
	if( count != 3 )
	{
		abort();
	}
	
	

	
	int i;
	int valuelen = 0;
	int flag = 0;
	char key[MPI_MAX_INFO_KEY];
	char * value = malloc( MPI_MAX_INFO_VAL );
	
	if( !value )
		abort();
		
	/* Test existing */
	for( i = 0 ; i < count ; i++ )
	{
		
		MPI_Info_get_nthkey( dupinfo , i, key );
		MPI_Info_get( dupinfo, key , MPI_MAX_INFO_VAL, value, &flag );
		
		if( strcmp( key, expected_keys[i]))
			abort();
		
		if( strcmp( value, expected_values[i]))
			abort();
		

	}
	
	/* CHECK CONTENT ORIGIN ========= */

	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );
	
	if( count != 3 )
	{
		abort();
	}
	
	if( !value )
		abort();
		
	/* Test existing */
	for( i = 0 ; i < count ; i++ )
	{
		
		MPI_Info_get_nthkey( info , i, key );
		MPI_Info_get( info, key , MPI_MAX_INFO_VAL, value, &flag );
		
		if( strcmp( key, expected_keys[i]))
			abort();
		
		if( strcmp( value, expected_values[i]))
			abort();
		

	}
	
	free( value );
	
	MPI_Info_free( &info );
	MPI_Info_free( &dupinfo );
		
	MPI_Finalize();

	return 0;
}
