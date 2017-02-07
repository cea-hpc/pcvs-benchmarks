#include <mpc.h>
#include <omp.h>
extern void abort (void);

struct Y
{
  int l[5][10];
};

struct X
{
  struct Y y;
  float b[10];
};

void
parallel (int a, int b)
{
  int i, j;
  int n_threads = 0 ;
  struct X A[10][5];
  a = b = 3;

  for (i = 0; i < 10; i++)
    for (j = 0; j < 5; j++)
      A[i][j].y.l[3][3] = -10;

  #pragma omp parallel shared (a, b, A) num_threads (5)
    {
      int i, j;

#pragma omp master
	  {
		  n_threads = omp_get_num_threads() ;
	  }

      #pragma omp atomic
      a += omp_get_num_threads ();

      #pragma omp atomic
      b += omp_get_num_threads ();

      #pragma omp for private (j)
      for (i = 0; i < 10; i++)
	for (j = 0; j < 5; j++)
	  A[i][j].y.l[3][3] += 20;

    }

  for (i = 0; i < 10; i++)
    for (j = 0; j < 5; j++)
      if (A[i][j].y.l[3][3] != 10)
	abort ();

  if (a != (3+n_threads*n_threads))
    abort ();

  if (b != (3+n_threads*n_threads))
    abort ();
}

main()
{
  parallel (1, 2);
  return 0;
}

