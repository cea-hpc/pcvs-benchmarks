#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    MPI_Comm var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Comm_compare(var_0, var_1, var_2);
    ret = PMPI_Comm_compare(var_0, var_1, var_2);
    return 0;
}
