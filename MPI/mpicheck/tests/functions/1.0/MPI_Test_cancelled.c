#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_Status *var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Test_cancelled(var_0, var_1);
    ret = PMPI_Test_cancelled(var_0, var_1);
    return 0;
}
