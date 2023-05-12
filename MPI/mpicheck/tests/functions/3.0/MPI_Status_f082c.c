#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_F08_status *var_0;
    MPI_Status *var_1;
    int ret;
    /* calls */
    ret = MPI_Status_f082c(var_0, var_1);
    ret = PMPI_Status_f082c(var_0, var_1);
    return 0;
}
