#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    int ret;
    /* calls */
    ret = MPI_Free_mem(var_0);
    ret = PMPI_Free_mem(var_0);
    return 0;
}
