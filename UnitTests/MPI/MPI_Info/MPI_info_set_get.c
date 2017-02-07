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


	/* GET ========= */
	char t1[128];
	char t2[128];
	char t3[128];

	int flag = 0;

	/* Existing */
	MPI_Info_get( info, "test1" , 128, t1, &flag );
	printf("%s == %d\n", t1, flag );
	
	if( flag != 1 )
		abort();
	
	if( strcmp( t1 , test1_string ) )
		abort();

	/* Existing */
	MPI_Info_get( info, "test2" , 128, t2, &flag );
	printf("%s == %d\n", t2, flag );

	if( flag != 1 )
		abort();

	if( strcmp( t2 , test2_string ) )
		abort();


	/* Existing */
	MPI_Info_get( info, "test3" , 128, t3, &flag );
	printf("%s == %d\n", t3, flag );

	if( flag != 1 )
		abort();

	if( strcmp( t3 , test3_string ) )
		abort();


	/* Not existing */
	MPI_Info_get( info, "test9" , 128, t1, &flag );
	printf("%s == %d\n", t1, flag );

	if( flag != 0 )
		abort();
	
	MPI_Info_free( &info );
		
	MPI_Finalize();

	return 0;
}
