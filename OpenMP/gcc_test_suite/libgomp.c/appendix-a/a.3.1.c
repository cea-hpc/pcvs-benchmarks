/* { dg-do run } */

#include <stdio.h>
#include <omp.h>


int
main ()
{
# ifdef _OPENMP
  printf ("Compiled by an OpenMP-compliant implementation.\n");
# endif
  return 0;
}
