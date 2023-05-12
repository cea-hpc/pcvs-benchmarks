#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    MPI_Comm var_2;
    int *var_3;
    MPI_Message *var_4;
    MPI_Status *var_5;
    int ret;
    /* calls */
    ret = MPI_Improbe(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Improbe(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
