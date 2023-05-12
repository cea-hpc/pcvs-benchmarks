#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Op *var_0;
    int ret;
    /* calls */
    ret = MPI_Op_free(var_0);
    ret = PMPI_Op_free(var_0);
    return 0;
}
