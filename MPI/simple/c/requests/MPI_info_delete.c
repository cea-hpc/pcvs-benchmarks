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
	const char *test1_string = "THIS IS TEST 1";
	
	int ret = 	MPI_Info_set( info, "test1", test1_string);
	
	if( ret != MPI_SUCCESS )
	{
		printf("Error setting an MPI_Info_key\n");
		abort();
	}
	


	/* GET ========= */
	char t1[128];

	int flag = 0;

	/* Existing */
	MPI_Info_get( info, "test1" , 128, t1, &flag );
	printf("%s == %d\n", t1, flag );
	
	if( flag != 1 )
		abort();
	
	if( strcmp( t1 , test1_string ) )
		abort();

	MPI_Info_delete( info , "test1"); 
	
	/* Deleted */
	MPI_Info_get( info, "test1" , 128, t1, &flag );
	printf("%s == %d\n", t1, flag );

	if( flag != 0 )
		abort();
		
	
	MPI_Info_free( &info );
		
	MPI_Finalize();

	return 0;
}
