#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Finalized(var_0);
    ret = PMPI_Finalized(var_0);
    return 0;
}
