#include <omp.h>
#include <stdio.h>

int main( int argc, char ** argv ) {
  int r = 45 ; 
#pragma omp parallel
  {
    printf( "Hello from %d / %d (r = %d)\n", omp_get_thread_num(), omp_get_num_threads(), r ) ;
  }
  return 0 ;
}
