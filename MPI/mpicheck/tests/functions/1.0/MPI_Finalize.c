#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int ret;
    /* calls */
    ret = MPI_Finalize();
    ret = PMPI_Finalize();
    return 0;
}
