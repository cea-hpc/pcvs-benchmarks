#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main() {

  int r = 45 ; 
  int * array ;
  int N = 50 ;
  int i ;
  int ok ;
  int cs = 1 ;

  /* INITIALIZATION */
  array = (int *)malloc( N * sizeof( int ) ) ;

  for ( i = 0 ; i < N ; i++ ) {
    array[i] = 0 ;
  }


  for ( cs = 1 ; cs < N+1 ; cs++ ) {
  /* KERNEL */
#pragma omp parallel private(i)
  {
#pragma omp for schedule(static,cs) 
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
  }

  /* VERIFICATION */
  ok = 1 ;
  if ( array[0] != 2*N ) {
    printf( "ERROR: array[%d] = %d (value %d expected)\n", 0, array[0], 2*N ) ;
    ok = 0 ;
  }
  for ( i = 1 ; i < N ; i++ ) {
    if ( array[i] != N ) {
      printf( "ERROR: array[%d] = %d (value %d expected)\n", i, array[i], N ) ;
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

