/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include <stdio.h>
#include <assert.h>
#include "mpi.h"


int main(int argc, char *argv[])
{
    int size, rank;
    int local_rank, local_size;
    MPI_Comm split_comm, inter_comm;
    int buffer = 0;

    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm_split(MPI_COMM_WORLD, rank % 2, rank, &split_comm);

    MPI_Barrier(split_comm);
    MPI_Comm_size(split_comm, &local_size);
    MPI_Comm_rank(split_comm, &local_rank);

    if (rank == 0)
        MPI_Intercomm_create(MPI_COMM_SELF, 0, MPI_COMM_WORLD, 1, 0, &inter_comm);

    if (rank % 2)
        MPI_Intercomm_create(split_comm, 0, MPI_COMM_WORLD, 0, 0, &inter_comm);

    if (rank == 0) {
        MPI_Abort(inter_comm, 8);
    }

    if (rank == 0) {
        buffer = 4;
        MPI_Send(&buffer, 1, MPI_INT, 1, 0, split_comm);
    }

    if (rank == 2) {
        MPI_Recv(&buffer, 1, MPI_INT, 0, 0, split_comm, MPI_STATUS_IGNORE);
        assert(buffer == 4);
        printf("No Errors\n");
        fflush(stdout);
    }

    MPI_Comm_free(&split_comm);

    MPI_Finalize();

    return 0;
}
