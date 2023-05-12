#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Keyval_free(var_0);
    ret = PMPI_Keyval_free(var_0);
    return 0;
}
