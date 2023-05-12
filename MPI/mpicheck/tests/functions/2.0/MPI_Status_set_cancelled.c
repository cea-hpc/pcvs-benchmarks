#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Status *var_0;
    int var_1;
    int ret;
    /* calls */
    ret = MPI_Status_set_cancelled(var_0, var_1);
    ret = PMPI_Status_set_cancelled(var_0, var_1);
    return 0;
}
