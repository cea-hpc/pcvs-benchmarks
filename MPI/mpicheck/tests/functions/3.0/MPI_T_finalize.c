#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int ret;
    /* calls */
    ret = MPI_T_finalize();
    ret = PMPI_T_finalize();
    return 0;
}
