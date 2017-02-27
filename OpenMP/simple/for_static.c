#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main() {

  int r = 45 ; 
  int * array ;
  int N = 50 ;
  long i ;
  int ok ;

  /* INITIALIZATION */
  array = (int *)malloc( N * sizeof( int ) ) ;

  for ( i = 0 ; i < N ; i++ ) {
    array[i] = 0 ;
  }


  /* KERNEL */
#pragma omp parallel private(i)
  {
#pragma omp for schedule(static) 
    for ( i = 0 ; i < N ; i++ ) {
#pragma omp atomic
      array[i]++ ;
    }

#pragma omp single
    {
#pragma omp atomic
      array[0]++ ;
    }

  }

  /* VERIFICATION */
  ok = 1 ;
  if ( array[0] != 2 ) {
    printf( "ERROR: array[%d] = %d (value 2 expected)\n", 0, array[0] ) ;
    ok = 0 ;
  }
  for ( i = 1 ; i < N ; i++ ) {
    if ( array[i] != 1 ) {
      printf( "ERROR: array[%d] = %d (value 1 expected)\n", i, array[i] ) ;
      ok = 0 ;
    }
  }


  /* OUTPUT */
  if ( ok != 1 ) {
    printf( "FAILED\n" ) ;
    abort() ;
  }

  printf( "SUCCESS\n" ) ;
  return 0 ;
}

