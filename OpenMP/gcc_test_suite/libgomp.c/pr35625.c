#include <mpc.h>
#include <omp.h>
/* PR libgomp/35625 */
/* { dg-do run } */
/* { dg-options "-std=c99" } */

int
main (void)
{
 int i,j;
#pragma omp parallel private(i,j)
  {
    #pragma omp for schedule (guided, 10)
      for (i = 0; i < 1826; i += 10)
	;
    #pragma omp for schedule (guided, 10)
      for (i = 0; i > -1826; i -= 10)
	;
  }
  return 0;
}
