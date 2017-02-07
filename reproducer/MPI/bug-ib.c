#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char **argv)
{
    int err;
    int my_rank;
    char *a;
    MPI_Datatype t;

    err = MPI_Init(&argc, &argv);
    assert(err == MPI_SUCCESS);

    err = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    assert(err == MPI_SUCCESS);

    a = malloc(8 * 1024 * sizeof(*a));
    assert(a != NULL);

    err = MPI_Type_vector(1, 8192, 0, MPI_CHAR, &t);
    assert(err == MPI_SUCCESS);

    err = MPI_Type_commit(&t);
    assert(err == MPI_SUCCESS);

    if (my_rank == 0)
    {
        err  = MPI_Send(a, 1, t, 1, 0, MPI_COMM_WORLD);
        assert(err == MPI_SUCCESS);
    }
    else if (my_rank == 1)
    {
        err = MPI_Recv(a, 1, t, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        assert(err == MPI_SUCCESS);
    }

    err = MPI_Type_free(&t);
    assert(err == MPI_SUCCESS);

    free(a);

    err = MPI_Finalize();
    assert(err == MPI_SUCCESS);

    return EXIT_SUCCESS;
}
