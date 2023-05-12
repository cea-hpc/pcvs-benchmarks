#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Add_error_code(var_0, var_1);
    ret = PMPI_Add_error_code(var_0, var_1);
    return 0;
}
