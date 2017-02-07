#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

int main(int argc, char **argv)
{
    int err;
    int my_rank;

    err = MPI_Init(&argc, &argv);
    assert(err == MPI_SUCCESS);

    err = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    assert(err == MPI_SUCCESS);

    if (my_rank == 0)
    {
        char c1 = 42;
        char c2 = 24;
        char buff[128];
        int pos = 0;

        err = MPI_Pack(&c1, 1, MPI_CHAR, buff, 128, &pos, MPI_COMM_WORLD);
        assert(err == MPI_SUCCESS);

        err = MPI_Pack(&c2, 1, MPI_CHAR, buff, 128, &pos, MPI_COMM_WORLD);
        assert(err == MPI_SUCCESS);

        err = MPI_Send(buff, pos, MPI_PACKED, 1, 0, MPI_COMM_WORLD);
        assert(err == MPI_SUCCESS);
    }
    else if (my_rank == 1)
    {
        char c[2];
        MPI_Datatype t;

        err = MPI_Type_vector(1, 2, 0, MPI_CHAR, &t);
        assert(err == MPI_SUCCESS);

        err = MPI_Type_commit(&t);
        assert(err == MPI_SUCCESS);

        err = MPI_Recv(&c, 1, t, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        assert(err == MPI_SUCCESS);

        err = MPI_Type_free(&t);
        assert(err == MPI_SUCCESS);

        printf("Recv %d %d\n", c[0], c[1]);
    }

    err = MPI_Finalize();
    assert(err == MPI_SUCCESS);

    return EXIT_SUCCESS;
}
