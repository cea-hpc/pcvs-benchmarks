#include <mpi.h>

int main(int argc, const char *argv[])
{
    MPI_Comm test;
    MPI_Init(&argc,&argv);
    MPI_Comm_dup( MPI_COMM_WORLD, &test);
    MPI_Finalize();
    return 0;
}
