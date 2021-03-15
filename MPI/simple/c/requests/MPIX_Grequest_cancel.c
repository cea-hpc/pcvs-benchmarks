#include <mpi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int query_fn(  void * extra_state, MPI_Status * status )
{
	return MPI_SUCCESS;
}

int free_called = 0;

int free_fn( void * extra_state )
{
	printf("Free called ! \n");
	
	free_called = 1;

	return MPI_SUCCESS;
}

int cancel_called = 0;

int cancel_fn( void *extra_state, int complete )
{
	printf("Cancel called ! \n");
	
	cancel_called = 1;

	return MPI_SUCCESS;
}

int poll_fn( void * preq, MPI_Status * status )
{
	MPI_Request * req = (MPI_Request *) preq;

	sleep(1);

	MPI_Grequest_complete( *req );	

	pthread_exit( NULL );
}

int main( int argc, char **argv )
{
	MPI_Init( &argc, &argv );

	MPI_Request my_req;
	int param = 0;

	pthread_t progress;

	MPIX_Grequest_start( query_fn, free_fn, cancel_fn, poll_fn, (void*)&param, &my_req );

	MPI_Cancel( &my_req );
	
	MPI_Wait( &my_req, MPI_STATUS_IGNORE );
	
	if( cancel_called != 1 )
	{
		printf("Error cancel was not called!\n");
		abort();
	}
	
	if( free_called != 1 )
	{
		printf("Error free was not called!\n");
		abort();
	}

	MPI_Finalize();
}
