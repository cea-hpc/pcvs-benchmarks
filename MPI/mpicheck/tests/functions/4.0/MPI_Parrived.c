#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request var_0;
    int var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Parrived(var_0, var_1, var_2);
    ret = PMPI_Parrived(var_0, var_1, var_2);
    return 0;
}
