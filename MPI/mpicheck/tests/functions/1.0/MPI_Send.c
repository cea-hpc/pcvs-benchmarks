#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Datatype var_2;
    int var_3;
    int var_4;
    MPI_Comm var_5;
    int ret;
    /* calls */
    ret = MPI_Send(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Send(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
