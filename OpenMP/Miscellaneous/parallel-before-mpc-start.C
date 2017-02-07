#include <mpc.h>
#include <omp.h>
#include <stdio.h>

extern "C" void abort (void);

struct S
{
  S ();
  ~S ();
  S (const S &);
  int i;
};

int n[2] = { 0, 0 };

S::S () : i(18)
{
#pragma omp parallel
  {
#pragma omp atomic
    n[0]++;
  }
}

S::~S ()
{
#pragma omp parallel
  {
#pragma omp atomic
    n[1]++;
  }
}

S
foo()
{
  S ret;

  return ret;
}

S x;

int
main (void)
{
  int num_threads = 0;

  #pragma omp parallel
  {
#pragma omp master
    num_threads = omp_get_num_threads();
  }

  if (n[0] < 1 || n[0] > num_threads || n[1] != 0){
    abort ();
  }

  x = foo();

  if (n[0] < 2 || n[0] > num_threads * 2 || n[1] < 1 || n[1] > num_threads){
    abort ();
  }

  {
    S y;

    if (n[0] < 3 || n[0] > num_threads * 3 || n[1] < 1 || n[1] > num_threads){
      abort ();
    }
  }

  if (n[0] < 3 || n[0] > num_threads * 3 || n[1] < 2 || n[1] > num_threads * 2){
    abort ();
  }

  return 0;
}
