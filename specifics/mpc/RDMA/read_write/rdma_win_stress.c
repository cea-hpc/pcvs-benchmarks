#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>


sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );
void sctk_window_RDMA_write( sctk_window_t win_id, void * src_addr, size_t size, size_t dest_offset, sctk_request_t  * req  );
void sctk_control_message_process();


/*
 * The purpose of this test is to validate the window mechanism
 * with a particular interest in pinning to do so we pin
 * subpart of a large array to force MMU saturation, validating
 * the eviction and also checking that window refcounting does
 * its job */


#define COUNT 1024 * 16
#define SIZE 1024 * 1024 * 1024

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
	
	
    int l;


	/* This is the local array containing datas */
	int* array = malloc( SIZE * sizeof( int ) );

	if( array == NULL )
	{
		perror("malloc");
		return 1;
	}

    for( l = 1 ; l < SIZE - COUNT ; l+=COUNT )  
    {
        if(rank && l%(COUNT/20)==0)
            printf("\t - Iteration %d\n", l);
        int * local_array = array + l;

		unsigned long int i;

		/* Each process fill it with zeroes */
		for( i = 0 ; i < COUNT ; i+=COUNT )
			local_array[i] = 0;


		int local, remote;



		local = sctk_window_init( local_array, sizeof( int ) * COUNT, sizeof( int ), SCTK_COMM_WORLD );
		
		MPI_Barrier( MPI_COMM_WORLD );

		/* 0 Will map to 1's window */
		if( rank == 0 )
		{
			MPI_Recv( &remote, 1, MPI_INT, 1, 10, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
			
			/* Map a window to remote window, note that this call create a second window
			* in addition to the local one, window with the particularity of being
			* remote of course */
			remote = sctk_window_map_remote( 1,  remote );

		}
		else
		{
			MPI_Send( &local, 1, MPI_INT, 0, 10, MPI_COMM_WORLD );
		}
		
		MPI_Barrier( MPI_COMM_WORLD );
		

		if( ! rank )
			sctk_window_release( remote );
		
		sctk_window_release( local );
    }
	
	MPI_Finalize();
	

	free( array );
	return 0;
}
