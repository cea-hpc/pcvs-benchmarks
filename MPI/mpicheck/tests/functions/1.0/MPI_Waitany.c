#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Request var_1[2];
    int *var_2;
    MPI_Status *var_3;
    int ret;
    /* calls */
    ret = MPI_Waitany(var_0, var_1, var_2, var_3);
    ret = PMPI_Waitany(var_0, var_1, var_2, var_3);
    return 0;
}
