#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    const void *var_1;
    int ret;
    /* calls */
    ret = MPI_Win_detach(var_0, var_1);
    ret = PMPI_Win_detach(var_0, var_1);
    return 0;
}
