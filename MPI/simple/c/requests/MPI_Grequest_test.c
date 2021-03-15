#include <stdio.h>
#include <unistd.h>
#include <mpi.h>
#include <pthread.h>

int query_fn(  void * extra_state, MPI_Status * status )
{
	return MPI_SUCCESS;
}


pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
int free_called = 0;
int completed_called = 0;

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

void * progress_func( void * preq )
{
	MPI_Request * req = (MPI_Request *) preq;

	sleep(1);

	MPI_Grequest_complete( *req );	
	completed_called = 1;

	pthread_exit( NULL );
}

int main( int argc, char **argv )
{
	MPI_Init( &argc, &argv );

	MPI_Request my_req;
	int param = 0;

	pthread_t progress;

	MPI_Grequest_start( query_fn, free_fn, cancel_fn, (void*)&param, &my_req );
	pthread_create( &progress, NULL, progress_func, (void *)&my_req);

	MPI_Status status;
	
	
	int flag = 0;
	
	while( !completed_called ); /* query and free functions are called only after completion */
	while( !flag )
	{
		MPI_Test( &my_req, &flag, &status );
	}

	if( !free_called )
	{
		printf("Error request free was not called\n");
	}

	pthread_join( progress, NULL );	
	MPI_Finalize();
}
