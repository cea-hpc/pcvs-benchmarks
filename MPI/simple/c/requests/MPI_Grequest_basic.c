#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <mpi.h>
#include <pthread.h>

int query_called = 0;
int query_fn(  void * extra_state, MPI_Status * status )
{
	query_called = 1;
	return MPI_SUCCESS;
}

int free_called = 0;
int free_fn( void * extra_state )
{
	free_called = 1;
	return MPI_SUCCESS;
}

int cancel_fn( void *extra_state, int complete )
{
	return MPI_SUCCESS;
}

int completed_called = 0;
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
	while( !completed_called ); /* query and free functions are called only after completion */
	MPI_Wait( &my_req, &status );

	if(!query_called||!free_called )
	{
		printf("Error free or querry were not called\n");
		abort();
	}

	pthread_join( progress, NULL );	
	MPI_Finalize();
}
