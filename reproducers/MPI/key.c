#include <stdlib.h>

#include <mpi.h>

int main(int argc, char **argv)
{
    MPI_Init(&argc, &argv);

    int key;
    MPI_Keyval_create(MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, &key, NULL);

    int i = 42;
    MPI_Attr_put(MPI_COMM_WORLD, key, &i);

    int *j;
    int flag;
    MPI_Attr_get(MPI_COMM_WORLD, key, &j, &flag);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
