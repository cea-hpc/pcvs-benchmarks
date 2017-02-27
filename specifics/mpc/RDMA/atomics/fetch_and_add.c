#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>
#include <stdint.h>


sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win, void *addr, size_t size, size_t disp_unit );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );

void sctk_window_RDMA_fetch_and_op( sctk_window_t remote_win_id, size_t remote_offset, void * fetch_addr, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );
void sctk_window_RDMA_fetch_and_op_win( sctk_window_t remote_win_id, size_t remote_offset, sctk_window_t local_win_id, size_t fetch_offset, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );


#define SIZE 10
#define COUNT 16384

int main( int argc, char ** argv )
{
	MPI_Init( &argc, &argv );

	/* Set context rank and size */

	int rank, size;
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );
	
	/* Make sure we run on two processes */
	
	if( size != 2 )
		abort();

	/* Set peer rank 1 -> 0 and  0 -> 1 */
	int peer = -1;

	if( rank == 0 )
	{
		peer = 1;
	}
	else
	{
		peer = 0;
	}

	/* HERE WE START DOING RDMA WORK */

	sctk_window_t local, remote=-8;

	/* This is the local array containing datas */
	int local_value[SIZE];
	
	int i;
	
	for( i = 0 ; i < SIZE ; i++ )
		local_value[i] = 2;
	
	/* Put this local array in an RDMA window indexed in integers */
	local = sctk_window_init( local_value, sizeof( int ) * SIZE, sizeof( int ), SCTK_COMM_WORLD );

	/* Exchange win ids to be able to request a remote window (using rank + remote id) */
	MPI_Sendrecv( &local, 1, MPI_INT, peer, 0, &remote, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );

	/* Map a window to remote window, note that this call create a second window
	 * in addition to the local one, window with the particularity of being 
	 * remote of course */
	remote = sctk_window_map_remote( peer,  remote );
	
	/* Prepare to enter exchange phase */
	MPI_Barrier( MPI_COMM_WORLD );
	
	/* START RDMA phase */

	int expected_sum = 2;
	int expected_inc = 2;
	int expected_dec = 2;
	int expected_prod = 2;
	int expected_and = 2;
	int expected_or = 2;
	int expected_xor = 2;

	for( i = 0 ; i < COUNT ; i++ )
	{
        if(rank && i%(COUNT/20)==0)
            printf("\t - Iteration %d\n", i);
		sctk_request_t req;
		int result_sum, result_inc, result_dec, result_and = 2, result_or = 2, result_xor = 2, result_prod = 2;

		int add_s = rank + 1;
		int dummy = 0;
		int two = 2;

		sctk_window_RDMA_fetch_and_op( remote, 0, &result_sum, &add_s, RDMA_SUM, RDMA_TYPE_INT, &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 1, &result_inc, NULL, RDMA_INC,RDMA_TYPE_INT,  &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 2, &result_dec, NULL, RDMA_DEC,RDMA_TYPE_INT,  &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 3, &result_prod, &two, RDMA_PROD,RDMA_TYPE_INT,  &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 4, &result_and, &result_and, RDMA_BAND,RDMA_TYPE_INT,  &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 5, &result_or, &result_sum, RDMA_BOR, RDMA_TYPE_INT, &req );
		sctk_wait_message ( &req );
		sctk_window_RDMA_fetch_and_op( remote, 6, &result_xor, &result_sum, RDMA_BXOR,RDMA_TYPE_INT,  &req );

		/* Wiat it directly */
		sctk_wait_message ( &req );
	
				
		if( result_sum != expected_sum )
		{
			printf("Error (%d) %d != %d\n", rank, result_sum, expected_sum );
			abort();
		}
				
		if( result_inc != expected_inc )
		{
			printf("INC Error (%d) %d != %d\n", rank, result_inc, expected_inc );
			abort();
		}
				
		if( result_dec != expected_dec )
		{
			printf("DEC Error (%d) %d != %d\n", rank, result_dec, expected_dec );
			abort();
		}
			
		if( result_and != expected_and )
		{
			printf("AND Error (%d) %d != %d\n", rank, result_and, expected_and );
			abort();
		}
		
			
		if( result_prod != expected_prod )
		{
			printf("PROD Error (%d) %d != %d\n", rank, result_prod, expected_prod );
			abort();
		}
				
		if( result_or != expected_or )
		{
			printf("OR Error (%d) %d != %d\n", rank, result_or, expected_or );
			abort();
		}
		
						
		expected_sum+= rank + 1;
		expected_inc++;
		expected_dec--;
		expected_prod *= 2;
		expected_and &= result_and;
		expected_or |= result_sum;
		expected_xor ^= result_sum;
	}	


	

	MPI_Finalize();


	return 0;
}
