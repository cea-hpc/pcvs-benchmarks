#include <mpc.h>
#include <omp.h>
#include <omp.h>

extern "C" void abort (void);

int
foo (void)
{
  return 8;
}

main ()
{
  int A = 0;

  #pragma omp parallel if (foo () > 8) shared (A)
    {
      A = omp_get_num_threads ();
    }

  if (A != 1)
    abort ();

  #pragma omp parallel if (foo () == 8) num_threads (3) shared (A)
    {
      A = omp_get_num_threads ();
    }

  if (A != 3)
    abort ();

  #pragma omp parallel if (foo () == 8) num_threads (foo ()) shared (A)
    {
      A = omp_get_num_threads ();
    }

  if (A != 8)
    abort ();

  return 0;
}
