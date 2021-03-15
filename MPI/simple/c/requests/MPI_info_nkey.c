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
	const char *test2_string = "THIS IS TEST 2";
	const char *test3_string = "THIS IS TEST 3";
	
	int ret = 	MPI_Info_set( info, "test1", test1_string);
	
	if( ret != MPI_SUCCESS )
	{
		printf("Error setting an MPI_Info_key\n");
		abort();
	}
	
	MPI_Info_set( info, "test2", test2_string);
	MPI_Info_set( info, "test3", test3_string);


	/* NKEY ========= */

	printf("====================\n");

	printf("-----------\n");	

	int count = 0;
	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );

	if( count != 3 )
		abort();


	printf("-----------\n");	
	MPI_Info_delete( info , "test2"); 
	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );

	if( count != 2 )
		abort();


	printf("-----------\n");	
	MPI_Info_delete( info , "test1"); 
	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );

	if( count != 1 )
		abort();


	printf("-----------\n");

	printf("-----------\n");	
	MPI_Info_delete( info , "test3"); 
	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );

	if( count != 0 )
		abort();


	printf("-----------\n");	


	

	MPI_Info_set( info, "NewOne", "This is not finished !");
	
	printf("-----------\n");	
	MPI_Info_get_nkeys( info , &count );

	printf("Count : %d\n", count );

	if( count != 1 )
		abort();


	printf("-----------\n");

	return 0;
}
