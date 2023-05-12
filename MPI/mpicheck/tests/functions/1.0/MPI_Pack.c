#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Datatype var_2;
    void *var_3;
    int var_4;
    int *var_5;
    MPI_Comm var_6;
    int ret;
    /* calls */
    ret = MPI_Pack(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Pack(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
