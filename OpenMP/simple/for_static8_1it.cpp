#include <omp.h>
#include <cstdio>

int main() {

  int n = omp_get_num_threads() ;


#pragma omp parallel if(n)
  {

	  printf( "Before loop n=%d\n", n ) ;

#pragma omp for
  for ( long i = 0 ; i < n ; i++ ) {
    printf( "Hello w/ it %ld\n", i ) ;
  }

  }

  return 0 ;

}
