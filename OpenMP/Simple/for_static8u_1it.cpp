#include <omp.h>

int main() {

  int n = omp_get_num_threads() ;


#pragma omp parallel if(n)
  {

    printf( "Before loop n=%d\n", n ) ;

#pragma omp for
  for ( unsigned long i = 0 ; i < n ; i++ ) {
    printf( "Hello w/ it %d\n", i ) ;
  }

  }

  return 0 ;

}
