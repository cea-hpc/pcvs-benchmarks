#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Datatype var_2;
    void *var_3;
    int var_4;
    MPI_Datatype var_5;
    int var_6;
    MPI_Comm var_7;
    int ret;
    /* calls */
    ret = MPI_Scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
