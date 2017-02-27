#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>
#include <stdint.h>


sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win, void *addr, size_t size, size_t disp_unit );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );
void sctk_window_RDMA_fence( sctk_window_t win_id );

void sctk_window_RDMA_fetch_and_op( sctk_window_t remote_win_id, size_t remote_offset, void * fetch_addr, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );
void sctk_window_RDMA_fetch_and_op_win( sctk_window_t remote_win_id, size_t remote_offset, sctk_window_t local_win_id, size_t fetch_offset, void * add, RDMA_op op, RDMA_type type, sctk_request_t  * req );

void sctk_window_RDMA_CAS( sctk_window_t remote_win_id, size_t remote_offset, void * comp, void * new_data, void * res, RDMA_type type, sctk_request_t  * req );



#define COUNT 4096

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
	int local_value = 0;
	
	int i;

	
	/* Put this local array in an RDMA window indexed in integers */
	local = sctk_window_init( &local_value, sizeof( int ), sizeof( int ), SCTK_COMM_WORLD );

	/* Exchange win ids to be able to request a remote window (using rank + remote id) */
	MPI_Sendrecv( &local, 1, MPI_INT, peer, 0, &remote, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );

	/* Map a window to remote window, note that this call create a second window
	 * in addition to the local one, window with the particularity of being 
	 * remote of course */
	remote = sctk_window_map_remote( peer,  remote );
	
	/* Prepare to enter exchange phase */
	MPI_Barrier( MPI_COMM_WORLD );
	

	int expected_xor = 2;

	for( i = 0 ; i < COUNT ; i++ )
	{
        if(rank && i%(COUNT/20)==0)
            printf("\t - Iteration %d\n", i);
		int new_val = i + 1;
		sctk_request_t req;
		int res = 0;
		sctk_window_RDMA_CAS( remote, 0, &i, &new_val, &res,  RDMA_TYPE_INT, &req );
		sctk_wait_message ( &req );
	
	    if( res != i )
        {
            fprintf(stderr, "Error wrong CAS ret %d\n", res );
            abort();
        }

	}	

	sctk_window_RDMA_fence( remote );
	
	/* Barrier is needed in local as we must be sure that the other process is done */
	MPI_Barrier( MPI_COMM_WORLD );

	if( local_value != i )
	{
		printf("CAS Failed got %d\n", local_value );
		abort();
	}
	else
	{
		printf("CAS Worked\n");
	}
	

	MPI_Finalize();


	return 0;
}
