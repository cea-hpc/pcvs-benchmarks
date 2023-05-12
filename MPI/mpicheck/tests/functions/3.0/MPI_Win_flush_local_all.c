#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    int ret;
    /* calls */
    ret = MPI_Win_flush_local_all(var_0);
    ret = PMPI_Win_flush_local_all(var_0);
    return 0;
}
