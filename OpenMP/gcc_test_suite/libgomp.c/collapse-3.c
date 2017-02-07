#include <mpc.h>
#include <omp.h>
/* { dg-do run } */
/* { dg-options "-O2 -std=gnu99" } */

#include <string.h>
#include <stdlib.h>

int
main (void)
{
  int i2, l = 0;
  int a[3][3][3];
  /* Modified for C89 */
  int i, j, k ;

  memset (a, '\0', sizeof (a));
  #pragma omp parallel for collapse(4 - 1) schedule(static, 4)
    for (i = 0; i < 2; i++)
      for (j = 0; j < 2; j++)
	for (k = 0; k < 2; k++)
	  a[i][j][k] = i + j * 4 + k * 16;
  #pragma omp parallel private(k)
    {
      #pragma omp for collapse(2) reduction(|:l)
	for (i2 = 0; i2 < 2; i2++)
	  for (j = 0; j < 2; j++)
	    for (k = 0; k < 2; k++)
	      if (a[i2][j][k] != i2 + j * 4 + k * 16)
		l = 1;
    }
  if (l)
    abort ();
  return 0;
}
