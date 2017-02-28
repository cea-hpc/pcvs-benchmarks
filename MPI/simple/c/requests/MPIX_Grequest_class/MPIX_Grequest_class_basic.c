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

int poll_called = 0;
int poll_func( void * preq , MPI_Status * status )
{
	MPI_Request * req = (MPI_Request *) preq;

	poll_called++;

	int rank = 0;
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	
	fprintf(stderr, "Poll %d in %d\n",  poll_called, rank );

	if( poll_called == 2 )
		MPI_Grequest_complete( *req );	

	return MPI_SUCCESS;
}

int wait_fn_called = 0;

int wait_fn( int count, void ** array_of_states, double timeout, MPC_Status * status )
{
	wait_fn_called = 1;
	return MPI_SUCCESS;
}



int main( int argc, char **argv )
{
	MPI_Init( &argc, &argv );

	int rank = 0;
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	
	MPI_Request my_req;
	int param = 0;

	MPIX_Grequest_class class;

	MPIX_Grequest_class_create( query_fn, free_fn, cancel_fn, poll_func, wait_fn,&class);

	MPIX_Grequest_class_allocate( class, (void *)&my_req, &my_req );

	MPI_Status status;

	MPI_Wait( &my_req, &status );

	if(!query_called||!free_called || ( poll_called != 2 ) )
	{
		printf("RANK %d Error free or querry were not called (Q : %d  F  : %d  P : %d )\n", rank, query_called, free_called , poll_called);
		abort();
	}
	
	MPI_Finalize();
}
