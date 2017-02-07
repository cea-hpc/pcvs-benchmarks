#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>
#include <stdint.h>

sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );

void sctk_window_RDMA_fetch_and_op( sctk_window_t remote_win_id, size_t remote_offset, void * fetch_addr, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );
void sctk_window_RDMA_fetch_and_op_win( sctk_window_t remote_win_id, size_t remote_offset, sctk_window_t local_win_id, size_t fetch_offset, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );

#define SIZE 10
#define COUNT 256


#define CHECK_OP( RDMA_TYPE, TYPE1, TYPE2, TYPE3 ) \
int check_ ## TYPE1 ## TYPE2 ## TYPE3 ()\
{\
	int rank, size;\
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );\
	MPI_Comm_size( MPI_COMM_WORLD, &size );\
	\
	fprintf(stdout, "PROCESSING %s %s %s\n", #TYPE1, #TYPE2, #TYPE3 );\
	if( size != 2 )\
		abort();\
\
	int peer = -1;\
\
	if( rank == 0 )\
	{\
		peer = 1;\
	}\
	else\
	{\
		peer = 0;\
	}\
\
\
	TYPE1 TYPE2 TYPE3 var[SIZE];\
	\
	int i;\
\
	for( i = 0 ; i < SIZE ; i++ )\
	{\
		var[i] = 2;\
	}\
\
	sctk_window_t local, remote=-8;\
	\
\
	local = sctk_window_init( var, sizeof( TYPE1 TYPE2 TYPE3  ) * SIZE, sizeof( TYPE1 TYPE2 TYPE3  ), SCTK_COMM_WORLD );\
\
	MPI_Sendrecv( &local, 1, MPI_INT, peer, 0, &remote, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );\
\
	remote = sctk_window_map_remote( peer,  remote );\
\
	MPI_Barrier( MPI_COMM_WORLD );\
\
	TYPE1 TYPE2 TYPE3  expected_sum = 2;\
	TYPE1 TYPE2 TYPE3  expected_inc = 2;\
	TYPE1 TYPE2 TYPE3  expected_dec = 2;\
	TYPE1 TYPE2 TYPE3  expected_prod = 2;\
	TYPE1 TYPE2 TYPE3  expected_and = 2;\
	TYPE1 TYPE2 TYPE3  expected_or = 2;\
	TYPE1 TYPE2 TYPE3  expected_xor = 2;\
	TYPE1 TYPE2 TYPE3  expected_land = 2;\
	TYPE1 TYPE2 TYPE3  expected_lor = 2;\
	TYPE1 TYPE2 TYPE3  expected_lxor = 2;\
\
	for( i = 0 ; i < COUNT ; i++ )\
	{\
        if(rank && i%(COUNT/20)==0)\
            printf("\t - Iteration %d\n", i);\
		sctk_request_t req;\
		TYPE1 TYPE2 TYPE3  result_sum = 5, result_inc, result_dec, result_and = 2, result_or = 2, result_xor = 2,  result_land = 2, result_lor = 2, result_lxor = 2, result_prod = 2;\
\
		TYPE1 TYPE2 TYPE3  add_s = rank + 1;\
		TYPE1 TYPE2 TYPE3  dummy = 0;\
		TYPE1 TYPE2 TYPE3  two = 2;\
\
		sctk_window_RDMA_fetch_and_op( remote, 0, &result_sum, &add_s, RDMA_SUM, RDMA_TYPE, &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 1, &result_inc, NULL, RDMA_INC,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 2, &result_dec, NULL, RDMA_DEC,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 3, &result_prod, &two, RDMA_PROD,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 4, &result_and, &result_and, RDMA_BAND,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 5, &result_or, &result_sum, RDMA_BOR, RDMA_TYPE, &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 6, &result_xor, &result_sum, RDMA_BXOR,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 7, &result_land, &result_land, RDMA_LAND,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 8, &result_lor, &result_sum, RDMA_LOR, RDMA_TYPE, &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 9, &result_lxor, &result_sum, RDMA_LXOR,RDMA_TYPE,  &req );\
\
		sctk_wait_message ( &req );\
	\
				\
		if( result_sum != expected_sum )\
		{\
			printf("SUM Error (%d) %d != %d\n", rank, result_sum, expected_sum );\
			abort();\
		}\
				\
		if( result_inc != expected_inc )\
		{\
			printf("INC Error (%d) %d != %d\n", rank, result_inc, expected_inc );\
			abort();\
		}\
		\
		if( result_dec != expected_dec )\
		{\
			printf("DEC Error (%d) %d != %d\n", rank, result_dec, expected_dec );\
			abort();\
		}\
			\
		if( result_prod != expected_prod )\
		{\
			printf("PROD Error (%d) %d != %d\n", rank, result_prod, expected_prod );\
			abort();\
		}\
				\
			\
		if( result_and != expected_and )\
		{\
			printf("AND Error (%d) %d != %d\n", rank, result_and, expected_and );\
			abort();\
		}\
		\
		if( result_or != expected_or )\
		{\
			printf("OR Error (%d) %d != %d\n", rank, result_or, expected_or );\
			abort();\
		}\
				\
		if( result_xor != expected_xor )\
		{\
			printf("XOR Error (%d) %d != %d\n", rank, result_xor, expected_xor );\
			abort();\
		}\
		\
			\
		if( result_land != expected_land )\
		{\
			printf("AND Error (%d) %d != %d\n", rank, result_land, expected_land );\
			abort();\
		}\
		\
		if( result_lor != expected_lor )\
		{\
			printf("OR Error (%d) %d != %d\n", rank, result_lor, expected_lor );\
			abort();\
		}\
				\
		if( result_lxor != expected_lxor )\
		{\
			printf("LXOR Error (%d) %d != %d\n", rank, result_lxor, expected_lxor );\
			abort();\
		}\
		\
			\
		expected_sum+= rank + 1;\
		expected_inc++;\
		expected_dec--;\
		expected_prod *= 2;\
		expected_and &= result_and;\
		expected_or |= result_sum;\
		expected_xor ^= result_sum;\
		expected_land = result_land && expected_land;\
		expected_lor = result_lor || result_sum;\
		expected_lxor = (!(result_lxor) != (!(result_sum)));\
	}\
	\
	MPI_Barrier( MPI_COMM_WORLD );\
	\
	sctk_window_release( remote );\
	sctk_window_release( local );\
}

#define CHECK_OP_NOLOG( RDMA_TYPE, TYPE1, TYPE2, TYPE3 ) \
int check_ ## TYPE1 ## TYPE2 ## TYPE3 ()\
{\
	fprintf(stdout, "PROCESSING %s %s %s\n", #TYPE1, #TYPE2, #TYPE3 );\
	int rank, size;\
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );\
	MPI_Comm_size( MPI_COMM_WORLD, &size );\
	\
	if( size != 2 )\
		abort();\
\
	int peer = -1;\
\
	if( rank == 0 )\
	{\
		peer = 1;\
	}\
	else\
	{\
		peer = 0;\
	}\
\
\
	TYPE1 TYPE2 TYPE3 var[SIZE];\
	\
	int i;\
\
	for( i = 0 ; i < SIZE ; i++ )\
	{\
		var[i] = 2;\
	}\
\
	sctk_window_t local, remote=-8;\
	\
\
	local = sctk_window_init( var, sizeof( TYPE1 TYPE2 TYPE3  ) * SIZE, sizeof( TYPE1 TYPE2 TYPE3  ), SCTK_COMM_WORLD );\
\
	MPI_Sendrecv( &local, 1, MPI_INT, peer, 0, &remote, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );\
\
	remote = sctk_window_map_remote( peer,  remote );\
\
	MPI_Barrier( MPI_COMM_WORLD );\
\
	TYPE1 TYPE2 TYPE3  expected_sum = 2;\
	TYPE1 TYPE2 TYPE3  expected_inc = 2;\
	TYPE1 TYPE2 TYPE3  expected_dec = 2;\
	TYPE1 TYPE2 TYPE3  expected_prod = 2;\
\
	for( i = 0 ; i < COUNT ; i++ )\
	{\
        if(rank && i%(COUNT/20)==0)\
            printf("\t - Iteration %d\n", i);\
		sctk_request_t req;\
		TYPE1 TYPE2 TYPE3  result_sum = 5, result_inc, result_dec, result_and = 2, result_or = 2, result_xor = 2, result_prod = 2;\
\
		TYPE1 TYPE2 TYPE3  add_s = rank + 1;\
		TYPE1 TYPE2 TYPE3  dummy = 0;\
		TYPE1 TYPE2 TYPE3  two = 2;\
\
		sctk_window_RDMA_fetch_and_op( remote, 0, &result_sum, &add_s, RDMA_SUM, RDMA_TYPE, &req );\
		sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 1, &result_inc, NULL, RDMA_INC,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 2, &result_dec, NULL, RDMA_DEC,RDMA_TYPE,  &req );\
				sctk_wait_message ( &req );\
		sctk_window_RDMA_fetch_and_op( remote, 3, &result_prod, &two, RDMA_PROD,RDMA_TYPE,  &req );\
\
		sctk_wait_message ( &req );\
	\
				\
		if( result_sum != expected_sum )\
		{\
			printf("SUM Error (%d) %d != %d\n", rank, result_sum, expected_sum );\
			abort();\
		}\
				\
		if( result_inc != expected_inc )\
		{\
			printf("INC Error (%d) %d != %d\n", rank, result_inc, expected_inc );\
			abort();\
		}\
		\
		if( result_dec != expected_dec )\
		{\
			printf("DEC Error (%d) %d != %d\n", rank, result_dec, expected_dec );\
			abort();\
		}\
			\
		\
			\
		expected_sum+= rank + 1;\
		expected_inc++;\
		expected_dec--;\
		expected_prod *= 2;\
	}\
	\
	MPI_Barrier( MPI_COMM_WORLD );\
	\
	sctk_window_release( remote );\
	sctk_window_release( local );\
}

/*
RDMA_OP_def( char, , )
RDMA_OP_def_nobin( double, )
RDMA_OP_def_nobin( float, )
RDMA_OP_def( long, , )
RDMA_OP_def_nobin( long, double )
RDMA_OP_def( long, long ,)
RDMA_OP_def( long, long , int )
RDMA_OP_def( short, ,)
RDMA_OP_def( signed, char,)
RDMA_OP_def( unsigned, ,)
RDMA_OP_def( unsigned, char, )
RDMA_OP_def( unsigned, long , )
RDMA_OP_def( unsigned, long , long )
RDMA_OP_def( unsigned, short , )
RDMA_OP_def( sctk_wchar_t, , )
*/

CHECK_OP( RDMA_TYPE_CHAR, char, , ) 
CHECK_OP_NOLOG( RDMA_TYPE_DOUBLE, double, , ) 
CHECK_OP_NOLOG( RDMA_TYPE_FLOAT, float, , ) 
CHECK_OP( RDMA_TYPE_LONG, long, , ) 
CHECK_OP_NOLOG( RDMA_TYPE_LONG_DOUBLE, long, double , ) 
CHECK_OP( RDMA_TYPE_LONG_LONG, long, long, ) 
CHECK_OP( RDMA_TYPE_LONG_LONG_INT, long, long, int) 
CHECK_OP( RDMA_TYPE_SHORT, short, , ) 
CHECK_OP( RDMA_TYPE_SIGNED_CHAR, signed, char, ) 
CHECK_OP( RDMA_TYPE_UNSIGNED, unsigned, , ) 
CHECK_OP( RDMA_TYPE_UNSIGNED_CHAR, unsigned, char , ) 
CHECK_OP( RDMA_TYPE_UNSIGNED_LONG, unsigned, long , ) 
CHECK_OP( RDMA_TYPE_UNSIGNED_LONG_LONG, unsigned, long , long) 
CHECK_OP( RDMA_TYPE_UNSIGNED_SHORT, unsigned, short , ) 


int main( int argc, char ** argv )
{
	
	MPI_Init( &argc, &argv );
	

	check_char();
	check_double();
	check_float();
	check_long();
	check_longdouble();
	check_longlong();
	check_longlongint();
	check_short();
	check_signedchar();
	check_unsigned();
	check_unsignedchar();
	check_unsignedlong();
	
	MPI_Finalize();
}
