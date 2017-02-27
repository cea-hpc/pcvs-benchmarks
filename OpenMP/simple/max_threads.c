#include <stdio.h>
#include <omp.h>

int main() 
{
  int max_t ;
  int n_t ;
  int fixed_t ;
  int i ; 


  for ( i = 0 ; i < 256 ; i++ ) {

    max_t = omp_get_max_threads() ;

#pragma omp parallel
    {
#pragma omp master
      {
	n_t = omp_get_num_threads() ;
      }
    }

    if ( max_t != n_t ) {
      printf( "1 - Iteration %d - max_t=%d, n_t=%d\n", 
	  i, max_t, n_t ) ;
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

    max_t = omp_get_max_threads() ;


    if ( max_t != n_t ) {
      printf( "2 - Iteration %d - max_t=%d, n_t=%d\n", 
	  i, max_t, n_t ) ;
      abort() ;
    }
#if 0
    if ( fixed_t > n_t ) {
      printf( "3 - Iteration %d - fixed_t=%d, n_t=%d\n", 
	  i, fixed_t, n_t ) ;
      abort() ;
    }
#endif

  }

  for ( i = 0 ; i < 256 ; i++ ) {

    max_t = omp_get_max_threads() ;

#pragma omp parallel
    {
#pragma omp master
      {
	n_t = omp_get_num_threads() ;
      }
    }

    if ( max_t != n_t ) {
      printf( "4 - Iteration %d - max_t=%d, n_t=%d\n", 
	  i, max_t, n_t ) ;
      abort() ;
    }

  }

  return 0 ;
}
