#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main() 
{
  int max_runtime_t ;
  int max_t ;
  int max_nest_t;
  int nest_t;
  int fixed_t;
  int t_limit;
  int i;
  int num_threads = getenv( "OMP_NUM_THREADS" ) ? atoi( getenv( "OMP_NUM_THREADS" )) : 0;

  t_limit = omp_get_thread_limit();

  for ( i = 0 ; i < 256 ; i++ ) {

/*
 * This assumes the runtime implementation determines an optimale number of
 * threads greater than one for the next parallel region
 */

    max_runtime_t = omp_get_max_threads() ;

#pragma omp parallel
    {
#pragma omp master
      {
        max_t = omp_get_num_threads() ;
      }

      max_nest_t = omp_get_max_threads() ;

#pragma omp parallel
      {
#pragma omp master
        {
          nest_t = omp_get_num_threads() ;
        }
      }
#pragma omp flush(nest_t)

      if ( nest_t != 1 ) {
        printf( "1.1 - Nested loop with max 1 != %d \n", nest_t );
        abort() ;
      }
    }

    if ( max_t > max_runtime_t ) {
      printf( "1.2 - Iteration %d - max_runtime_t=%d, max_t=%d\n", 
	    i, max_runtime_t, max_t ) ;
      abort() ;
    }
  }


  for ( i = 1 ; i < 256 ; i++ ) {

#pragma omp parallel num_threads(i)
    {
#pragma omp master
      {
	    fixed_t = omp_get_num_threads() ;
      }
    }

    max_runtime_t = omp_get_max_threads() ;

    if ( max_t > max_runtime_t ) {
      printf( "2.1 - Iteration %d - max_runtime_t=%d, max_t=%d\n", 
	  i, max_runtime_t, max_t ) ;
      abort() ;
    }

    if( fixed_t < max_t && fixed_t != i ){
      printf( "2.2 - Iteration %d - max_t=%d, fixed_t=%d, i=%d\n", i, max_t, fixed_t, i );
      abort();
    }

    if( fixed_t > t_limit ){
      printf( "2.3 - Iteration %d - fixed_t=%d, t_limit=%d\n", i, fixed_t, t_limit );
      abort();
    }
#if 0
    if ( fixed_t > n_t ) {
      printf( "3 - Iteration %d - fixed_t=%d, n_t=%d\n", 
	  i, fixed_t, n_t ) ;
      abort() ;
    }
#endif

  }

  for ( i = 1 ; i < 256 ; i++ ) {

    omp_set_num_threads( i );

#pragma omp parallel
    {
#pragma omp master
      {
	    fixed_t = omp_get_num_threads() ;
      }
    }

    max_runtime_t = omp_get_max_threads() ;

    if ( max_runtime_t != i ) {
      printf( "3.1 - Iteration %d - max_runtime_t=%d, i=%d\n", 
	  i, max_runtime_t, i );
      abort() ;
    }

    if( fixed_t < t_limit && fixed_t != i ){
      printf( "3.2 - Iteration %d - t_limit=%d, fixed_t=%d, i=%d\n", i, t_limit, fixed_t, i );
      abort();
    }

    if( fixed_t > t_limit ){
      printf( "3.3 - Iteration %d - fixed_t=%d, t_limit=%d\n", i, fixed_t, t_limit );
      abort();
    }

  }

  return 0 ;
}
