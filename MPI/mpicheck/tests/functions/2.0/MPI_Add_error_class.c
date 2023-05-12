#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Add_error_class(var_0);
    ret = PMPI_Add_error_class(var_0);
    return 0;
}
