#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    MPI_Group *var_1;
    int ret;
    /* calls */
    ret = MPI_Win_get_group(var_0, var_1);
    ret = PMPI_Win_get_group(var_0, var_1);
    return 0;
}
