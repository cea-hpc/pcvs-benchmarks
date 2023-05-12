#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    MPI_Count *var_1;
    int ret;
    /* calls */
    ret = MPI_Buffer_detach_c(var_0, var_1);
    ret = PMPI_Buffer_detach_c(var_0, var_1);
    return 0;
}
