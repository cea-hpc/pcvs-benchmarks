#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <pthread.h>

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

int query_called = 0;
int query_fn(  void * extra_state, MPI_Status * status )
{
	pthread_mutex_lock( &lock );	
	query_called++;
	pthread_mutex_unlock( &lock );
	
	return MPI_SUCCESS;
}

int free_called = 0;

int free_fn( void * extra_state )
{
	pthread_mutex_lock( &lock );	
	free_called++;
	pthread_mutex_unlock( &lock );

	printf("Free\n");
	
	if( 100 < free_called)
	{
		((void (*)())0x0)();
	}
	
	
	return MPI_SUCCESS;
}

int cancel_fn( void *extra_state, int complete )
{


	return MPI_SUCCESS;
}


volatile int th_counter = 0;

void * progress_func( void * preq )
{
	MPI_Request * req = (MPI_Request *) preq;
	printf("+%d ", *((int *)req));

	pthread_mutex_lock( &lock );	
	th_counter++;
	pthread_mutex_unlock( &lock );

	MPI_Grequest_complete( *req );	

	printf("-%d ", *((int *)req));


	pthread_exit( NULL );
}



void test_waitall()
{
	MPI_Request my_req[10];
	MPI_Status my_stat[10];
	
	int param = 0;
	int i;
	th_counter = 0;
	free_called = 0;
	query_called = 0;

	pthread_t progress;

	for( i = 0 ; i < 10 ;  i++ )
	{
		MPI_Grequest_start( query_fn, free_fn, cancel_fn, (void*)&param, &my_req[i] );
		pthread_create( &progress, NULL, progress_func, (void *)&my_req[i]);
	}

	MPI_Waitall( 10,  my_req, my_stat );


	pthread_mutex_lock( &lock );	
	if( th_counter != 10 )
	{
		printf("The counter was not incremented by every threads( got %d)\n", th_counter);
		abort();
	}

	if( free_called != 10 )
	{
		printf("All requests werent freed( got %d)\n", free_called);
		abort();
	}

	if( query_called != 10 )
	{
		printf("All requests werent queried( got %d)\n", query_called);
		abort();
	}
	pthread_mutex_unlock( &lock );		
	
	printf("Waitall success over 10 Generic requests\n");

	pthread_join( progress, NULL );	
}



void test_waitall_nostatusses()
{
	MPI_Request my_req[10];
	
	int param = 0;
	int i;
	th_counter = 0;
	free_called = 0;
	query_called = 0;

	pthread_t progress;

	for( i = 0 ; i < 10 ;  i++ )
	{
		MPI_Grequest_start( query_fn, free_fn, cancel_fn, (void*)&param, &my_req[i] );
		pthread_create( &progress, NULL, progress_func, (void *)&my_req[i]);
	}

	MPI_Waitall( 10,  my_req, MPI_STATUSES_IGNORE );


	pthread_mutex_lock( &lock );	
	if( th_counter != 10 )
	{
		printf("The counter was not incremented by every threads( got %d)\n", th_counter);
		abort();
	}

	if( free_called != 10 )
	{
		printf("All requests werent freed( got %d)\n", free_called);
		abort();
	}

	if( query_called != 10 )
	{
		printf("All requests werent queried( got %d)\n", query_called);
		abort();
	}
	pthread_mutex_unlock( &lock );	
	
	printf("Waitall success over 10 Generic requests\n");

	pthread_join( progress, NULL );	
}




int main( int argc, char **argv )
{
	MPI_Init( &argc, &argv );

	test_waitall();
	test_waitall_nostatusses();
	
	MPI_Finalize();
}
