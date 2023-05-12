#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Request var_1[2];
    int *var_2;
    int var_3[2];
    MPI_Status var_4[2];
    int ret;
    /* calls */
    ret = MPI_Testsome(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Testsome(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
