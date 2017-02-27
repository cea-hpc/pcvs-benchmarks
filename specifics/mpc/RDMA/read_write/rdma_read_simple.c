#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sctk_types.h>


sctk_window_t sctk_window_init( void *addr, size_t size, size_t disp_unit, sctk_communicator_t comm );
void sctk_window_release( sctk_window_t win );
int sctk_window_map_remote( int remote_rank, sctk_window_t win_id );
void sctk_window_RDMA_write( sctk_window_t win_id, void * src_addr, size_t size, size_t dest_offset, sctk_request_t  * req  );
void sctk_window_RDMA_read( sctk_window_t win_id, void * dest_addr, size_t size, size_t src_offset, sctk_request_t  * req  );
void sctk_control_message_process();

/* THIS IS A BASIC READ TEST */

unsigned long long int COUNT = 10;

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


    /* This is the local array containing datas */
    int* local_array = malloc( COUNT * sizeof( int ) );

    if( local_array == NULL )
    {
        perror("malloc");
        return 1;
    }
    fprintf(stderr, "SIZE is %g MB\n", (sizeof( int ) * COUNT) / ( 1024.0 * 1024.0) );

    unsigned long int i;

    /* Each process fill it with zeroes */
    for( i = 0 ; i < COUNT ; i++ )
        local_array[i] = 0;


    /* SETUP RMA WINS */

    sctk_window_t local, remote=-8;

    local = sctk_window_init( local_array, sizeof( int ) * COUNT, sizeof( int ), SCTK_COMM_WORLD );

    /* Rank 1 fills the buffer */

    /* Let zero fill its buffer with an increasing flow */
    if( rank )
    {
        for( i = 0 ; i < COUNT ; i++ )
        {
            local_array[i] = i;
        }
    }

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

    if( !rank )
    {
        /* Zero writes */
        sctk_request_t  req;

        fprintf(stderr, "[%d] Local Arr is at%p\n", rank, local_array );
        sctk_window_RDMA_read( remote, local_array, COUNT * sizeof( int ), 0, &req  );

        sctk_wait_message( &req );


        sctk_window_RDMA_fence( remote );		
    }


    MPI_Barrier( MPI_COMM_WORLD );

    unsigned long int prev = -1;

    while( local_array[ COUNT - 1 ] == 0 )
    {

        unsigned long int errnct = 0;
        for( i = 0 ; i < COUNT ; i++ )
        {
            if(  local_array[i] != i )
            {
                fprintf(stderr, "Error at %d = %d \n", i, local_array[i] );
                errnct++;
            }
        }

        if( errnct != prev )
        {
            fprintf(stderr,"=====================\n");
            fprintf(stderr,"PErr %lu\n" ,errnct );
            fprintf(stderr, "=====================\n");
            abort();
        }

        prev = errnct;

    }


    if( rank == 0 )
        sctk_window_release( remote ); 

    sctk_window_release( local ); 
    free( local_array );


    MPI_Finalize();


    return 0;
}


