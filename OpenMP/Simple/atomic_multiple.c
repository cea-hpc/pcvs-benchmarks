#include <omp.h>
#include <stdio.h>


int main( int argc, char ** argv ) {
	int n_threads = 0 ;
	int i ;

	double atomic1 = 0.0 ;
	double add_atomic1 = 1.2 ;
	double initial_atomic1 ;

	long atomic2 = 0 ;
	long add_atomic2 = 4 ;
	long initial_atomic2 ;

	int atomic3 = 1 ;
	int wr_atomic3 = 1234567890 ;
	int initial_atomic3 ;


#pragma omp parallel
	{
#pragma omp master
		{
			n_threads = omp_get_num_threads() ;
		}
	}

	printf( "Application with %d thread(s)\n", n_threads ) ;

	/******** ATOMIC 1 ******************/
	initial_atomic1 = atomic1 ;

#pragma omp parallel
	{
#pragma omp atomic
		atomic1 += add_atomic1 ;
	}

	/* Verification */
	for ( i = 0 ; i < n_threads ; i++ ) {
		initial_atomic1 += add_atomic1 ;
	}

	if ( initial_atomic1 != atomic1  ) {
		printf( "FAILED DOUBLE ADD\n" ) ;
		return 1 ;
	} 

	/******** ATOMIC 2 ******************/
	initial_atomic2 = atomic2 ;

#pragma omp parallel
	{
#pragma omp atomic
		atomic2 += add_atomic2 ;
	}

	/* Verification */
	for ( i = 0 ; i < n_threads ; i++ ) {
		initial_atomic2 += add_atomic2 ;
	}

	if ( initial_atomic2 != atomic2  ) {
		printf( "FAILED LONG ADD\n" ) ;
		return 1 ;
	} 

	/******** ATOMIC 3 ******************/
	initial_atomic3 = atomic3 ;

#pragma omp parallel
	{
#pragma omp atomic write
		atomic3 = wr_atomic3 + omp_get_thread_num() ;
	}

	/* Verification */
	if ( atomic3 < wr_atomic3 || atomic3 >= wr_atomic3+n_threads  ) {
		printf( "FAILED INT WR\n" ) ;
		return 1 ;
	} 

	printf( "PASSED\n" ) ;
	return 0 ;
}



