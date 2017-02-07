#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int main( int argc, char ** argv ) {
  int a = 45 ; 
  float b = 46.0 ; 
  int c = 47 ; 
  long d = 48 ; 
  double e = 49.0 ; 
  int f = 50 ; 
  int g = 51 ; 
  int h = 52 ; 
  int i = 53 ; 

#pragma omp parallel
  {

#if 0
    printf( "Hello from %d / %d (a=%d,b=%f,c=%d,d=%ld,e=%g,f=%d,g=%d,h=%d,i=%d)\n", 
	omp_get_thread_num(), omp_get_num_threads(), a, b, c, d, e, f, g, h, i ) ;
#endif

#pragma omp single nowait
    {
      if (
          (a != 45) ||
          (b != 46.0) ||
          (c != 47) ||
          (d != 48) ||
          (e != 49.0) ||
          (f != 50) ||
          (g != 51) ||
          (h != 52) ||
          (i != 53)  
         ) {
        abort() ;
      }
    }

#pragma omp single nowait
    {
      if (
          (a != 45) ||
          (b != 46.0) ||
          (c != 47) ||
          (d != 48) ||
          (e != 49.0) ||
          (f != 50) ||
          (g != 51) ||
          (h != 52) ||
          (i != 53)  
         ) {
        abort() ;
      }
    }

  }
  return 0 ;
}

