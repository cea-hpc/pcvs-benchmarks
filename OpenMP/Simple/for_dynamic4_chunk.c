#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main() {

  int r = 45 ; 
  int * array ;
  int N = 5000 ;
  int i ;
  int ok ;
  int cs ;

  /* INITIALIZATION */
  array = (int *)malloc( N * sizeof( int ) ) ;
  if ( array == NULL ) {
    return 1 ;
  }

  for ( i = 0 ; i < N ; i++ ) {
    array[i] = 0 ;
  }

  /* KERNEL */
  for ( cs = 1 ; cs < N+1 ; cs++ ) {
    // printf( "Processing chunk size %d / %d...\n", cs, N ) ;
#pragma omp parallel private(i)
    {
#pragma omp for schedule(dynamic,cs) 
      for ( i = 0 ; i < N ; i++ ) {
#pragma omp atomic
	array[i]++ ;
      }
    }

#pragma omp parallel private(i)
    {
#pragma omp for schedule(dynamic,cs) 
      for ( i = 0 ; i < N ; i++ ) {
#pragma omp atomic
	array[i]++ ;
      }
    }
  }



  /* VERIFICATION */
  ok = 1 ;
  for ( i = 0 ; i < N ; i++ ) {
    if ( array[i] != 2*N ) {
      printf( "ERROR: array[%d] = %d (expected %d)\n", i, array[i], 2*N ) ;
      ok = 0 ;
    }
  }


  /* OUTPUT */
  if ( ok != 1 ) {
    printf( "FAILED\n" ) ;
    return 1 ;
  }

  printf( "SUCCESS\n" ) ;
  return 0 ;
}

