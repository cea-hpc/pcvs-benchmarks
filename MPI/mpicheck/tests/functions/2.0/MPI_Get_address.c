#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    MPI_Aint *var_1;
    int ret;
    /* calls */
    ret = MPI_Get_address(var_0, var_1);
    ret = PMPI_Get_address(var_0, var_1);
    return 0;
}
