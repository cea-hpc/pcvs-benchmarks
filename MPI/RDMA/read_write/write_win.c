#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>

/* Test the low-level RDMA interface in write
 * to do so we fill rank dependent arrays
 * on two processes before exchanging data
 * using a storm of small RDMA messages
 * runnning in shared, TCP and IB allows
 * the testing of the three transport modes */

sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win, void *addr, size_t size, size_t disp_unit );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );
void sctk_window_RDMA_write( sctk_window_t win_id, void * src_addr, size_t size, size_t dest_offset, sctk_request_t  * req  );

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
	
	/* Here we fill an interger with a rank dependent value and
	 * send it to an offset in the remote local_array */
	if( rank )
	{
		for( i = 0 ; i < COUNT ; i++ )
		{
            if(rank && i%(COUNT/20)==0)
                printf("\t - Iteration %d\n", i);
			sctk_request_t req;
			/* This is the rank dependent value */
			local_array[i] = rank * i;
			/* Write in the remote array */
			sctk_window_RDMA_write_win( local, i, sizeof( int ),  remote, i, &req  );
			
			/* Directly wait the RDMA write */
			sctk_wait_message ( &req );
		}
	}

	/* Now we do a fence on the window. It is important
	 * to note  that this fence only guarantees that the
	 * data are coherent in the remote window (RDMA write)
	 * however in this test we want to know that the remote
	 * has finished writing to look at our local data
	 * not that we are done writing. It is for this reason that
	 * we added a sendrecv just after the write fence */
	sctk_window_RDMA_fence( remote );
	
	/* Remote write ended meaning that local data are ready to read */
	int dummy;
	MPI_Sendrecv( &remote, 1, MPI_INT, peer, 0, &dummy, 1, MPI_INT,  peer, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE );

	int err_count = 0;

	if( !rank )
	{
		/* Lets check the content verifying that rank
		 * dependent data are present in our buffer */
		for( i = 0 ; i < COUNT ; i++ )
		{
			/* See how we compare with peer instead of rank */
			if( local_array[i] != (peer * i ) )
			{
				err_count++;
				fprintf(stderr, "%d != %d\n",  local_array[i], (peer + i ) );
			}
	}
	
	}
	
	fprintf(stderr,"WRITE %d ERR %d\n", rank, err_count );

	/* If we have a single error we abort */
	if( err_count )
		abort();

	return 0;
}
