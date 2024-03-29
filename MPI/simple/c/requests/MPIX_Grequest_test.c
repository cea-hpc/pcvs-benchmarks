#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

int query_fn(  void * extra_state, MPI_Status * status )
{
	return MPI_SUCCESS;
}


pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
int free_called = 0;

int free_fn( void * extra_state )
{
	pthread_mutex_lock( &lock );	
	free_called++;
	pthread_mutex_unlock( &lock );

	return MPI_SUCCESS;
}

int cancel_fn( void *extra_state, int complete )
{


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


	MPIX_Grequest_start( query_fn, free_fn, cancel_fn, poll_fn, (void*)&my_req, &my_req );


	MPI_Status status;
	
	
	int flag = 0;
	
	while( !flag )
	{
		MPI_Test( &my_req, &flag, &status );
	}


	if( !free_called )
	{
		printf("Error request free was not called\n");
	}


	MPI_Finalize();
}
