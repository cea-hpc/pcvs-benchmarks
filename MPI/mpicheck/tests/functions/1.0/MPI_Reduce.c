#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    void *var_1;
    int var_2;
    MPI_Datatype var_3;
    MPI_Op var_4;
    int var_5;
    MPI_Comm var_6;
    int ret;
    /* calls */
    ret = MPI_Reduce(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Reduce(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
