#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    int var_1;
    int ret;
    /* calls */
    ret = MPI_Win_delete_attr(var_0, var_1);
    ret = PMPI_Win_delete_attr(var_0, var_1);
    return 0;
}
