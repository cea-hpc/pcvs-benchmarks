#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Buffer_detach(var_0, var_1);
    ret = PMPI_Buffer_detach(var_0, var_1);
    return 0;
}
