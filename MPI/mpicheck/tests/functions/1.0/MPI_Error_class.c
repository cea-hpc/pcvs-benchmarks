#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Error_class(var_0, var_1);
    ret = PMPI_Error_class(var_0, var_1);
    return 0;
}
