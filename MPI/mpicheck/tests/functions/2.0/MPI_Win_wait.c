#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    int ret;
    /* calls */
    ret = MPI_Win_wait(var_0);
    ret = PMPI_Win_wait(var_0);
    return 0;
}
