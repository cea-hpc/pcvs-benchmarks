#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request *var_0;
    MPI_Status *var_1;
    int ret;
    /* calls */
    ret = MPI_Wait(var_0, var_1);
    ret = PMPI_Wait(var_0, var_1);
    return 0;
}
