#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <assert.h>
int rank;

int
main (int argc, char **argv)
{
  int i;
  int local_rank;

  MPI_Init (&argc, &argv);

  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  MPI_Comm_rank (MPI_COMM_WORLD, &local_rank);

  MPI_Barrier (MPI_COMM_WORLD);


#pragma omp parallel for
  for (i = 0; i < 10; i++)
    {
      printf ("P %d i=%d, rank=(%d/%d)\n", rank, i, omp_get_thread_num (),
	      omp_get_num_threads ());
    }
  MPI_Barrier (MPI_COMM_WORLD);

  assert (local_rank == rank);

  MPI_Finalize ();

  return 0;
}
