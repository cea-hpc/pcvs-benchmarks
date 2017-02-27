//#include <mpc.h>
#include <omp.h>
#include <stdio.h>
/* PR c++/26943 */
/* { dg-do run } */

extern void abort (void);
extern void omp_set_dynamic (int);
int n = 6;

int
main (void)
{
  int i, x = 0;
  omp_set_dynamic (0);
  int num_thread_to_execute = 16;
  int max_thread_openmp = omp_get_max_threads();
 
  if(num_thread_to_execute > max_thread_openmp)
  {
    printf("[WARNING] Resize benchmark num threads : %d -> %d\n", num_thread_to_execute, max_thread_openmp);
    num_thread_to_execute = max_thread_openmp;
  }
#pragma omp parallel for num_threads (max_thread_openmp) firstprivate (n) lastprivate (n) \
			 schedule (static, 1) reduction (+: x)
  for (i = 0; i < num_thread_to_execute; i++)
    {
      if (n != 6)
	++x;
      n = i;
    }
  if (x || n != num_thread_to_execute-1)
    abort ();
  return 0;
}
