#include <omp.h>


int a = 8 ;
double b = 3.0 ;

#pragma omp threadprivate(a, b)


int main() {
  void ** threadprivate_address_a ;
  void ** threadprivate_address_b ;
  int n_threads ;
  int i ;
  int ok ;

#pragma omp parallel
  {
#pragma omp single
    {
      n_threads = omp_get_num_threads() ;

      threadprivate_address_a = (void **)malloc( n_threads * sizeof( void * ) ) ;
      threadprivate_address_b = (void **)malloc( n_threads * sizeof( void * ) ) ;
    }

    threadprivate_address_a[ omp_get_thread_num() ] = &a ;
    threadprivate_address_b[ omp_get_thread_num() ] = &b ;
  }

  ok = 1 ;
  for ( i = 1 ; i < n_threads ; i++ ) {
    if ( threadprivate_address_a[i] == threadprivate_address_a[0] ) {
      ok = 0 ;
    }
    if ( threadprivate_address_b[i] == threadprivate_address_b[0] ) {
      ok = 0 ;
    }
    /*
    printf( "Address of a for thread %d: %p\n", i, threadprivate_address_a[i] ) ;
    */
  }

  if ( ok == 0 ) {
    printf( "FAILED\n" ) ;
    return 1 ;
  }

  printf( "SUCCESS\n" ) ;
  return 0 ;

}
