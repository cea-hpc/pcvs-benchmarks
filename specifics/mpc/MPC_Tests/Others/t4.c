#include "mpi.h"
#include <assert.h>
#include <stdio.h>


int main(int argc, char* argv[])
{
  MPI_Init(&argc, &argv);
  int myrank;
  int mysize;
  MPI_Comm_size(MPI_COMM_WORLD, &mysize);
  MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

  if (myrank == 0) {
    int n = mysize-1;
    while (n--) {
      int msg;
      MPI_Status status;
      MPI_Recv(&msg, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
      printf("received %d from %d (tag=%d)\n", msg, status.MPI_SOURCE, status.MPI_TAG);
      assert(status.MPI_SOURCE > 0);
      assert(status.MPI_TAG > 0);
      }
  } else {
    int tag = myrank;
    int dst = 0;
    MPI_Send(&myrank, 1, MPI_INT, dst, tag, MPI_COMM_WORLD);
  }

  MPI_Finalize();
  return 0;
}
