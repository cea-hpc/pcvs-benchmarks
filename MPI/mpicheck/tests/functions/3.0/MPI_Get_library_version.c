#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    char *var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Get_library_version(var_0, var_1);
    ret = PMPI_Get_library_version(var_0, var_1);
    return 0;
}