#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>

/* Test the low-level RDMA interface in read
 * to do so we fill an array on 1
 * before reading it on 0
 * using a storm of small RDMA messages
 * runnning in shared, TCP and IB allows
 * the testing of the three transport modes */


sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win, void *addr, size_t size, size_t disp_unit );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );

void sctk_window_RDMA_write( sctk_window_t win_id, void * src_addr, size_t size, size_t dest_offset, sctk_request_t  * req  );
void sctk_window_RDMA_read( sctk_window_t win_id, void * dest_addr, size_t size, size_t src_offset, sctk_request_t  * req  );

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
	int local_array[COUNT];
	
	int i;
	
	/* Each process fill it with zeroes */
	for( i = 0 ; i < COUNT ; i++ )
		local_array[i] = 0;

	/* Put this local array in an RDMA window indexed in integers */
	local = sctk_window_init( local_array, sizeof( int ) * COUNT, sizeof( int ), SCTK_COMM_WORLD );

	/* Exchange win ids to be able to request a remote window (using rank + remote id) */
	MPI_Sendrecv( &local, 1, MPI_INT, peer, 0, &remote, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );

	/* Map a window to remote window, note that this call create a second window
	 * in addition to the local one, window with the particularity of being 
	 * remote of course */
	remote = sctk_window_map_remote( peer,  remote );
	
	/* Prepare to enter exchange phase */
	MPI_Barrier( MPI_COMM_WORLD );
	
	/* Fill arrays on the two ranks */
	for( i = 0 ; i < COUNT; i++ )
	{
		if( rank )
		{
			/* On 1 fill with i * 2 */
			local_array[i] = i *2;
		}
		else
		{
			/* On "0" we empty everything */
			local_array[i] = 0;
		}
		
	}

	/* START RDMA phase */
	if( !rank )
	{
		/* On "0" lets read it all */
		for( i = 0 ; i < COUNT ; i++ )
		{
            if(i%(COUNT/20)==0)
                printf("\t - Iteration %d\n", i);
			sctk_request_t req;
			/* Note how the dest address matches the offset in the window */
			sctk_window_RDMA_read_win( remote, i, sizeof( int ), local, i, &req );
			/* Wiat it directly */
			sctk_wait_message ( &req );
		}	
		
	}

	/* Here we just do a fence as we were only doing reads
	 * it means that local data are coherent indiferently
	 * from the state  of the remote process */
	sctk_window_RDMA_fence( remote );
	
	/* And now lets check that we have
	 * the full array in "0"  note that
	 * we also check in "1" as it is not
	 * supposed to have changed */
	
    int err_count = 0;

	/* Do we have i*2 on both ranks ?? */
	for( i = 0 ; i < COUNT ; i++ )
	{
		if( local_array[i] != ( i *2 ) )
		{
			err_count++;
			fprintf(stderr, "%d != %d\n",  local_array[i], (peer + i ) );
		}
	}
	
	fprintf(stderr,"READ %d ERR %d\n", rank, err_count );

	if( err_count )
	{
		sctk_abort();
	}


	MPI_Finalize();


	return 0;
}
