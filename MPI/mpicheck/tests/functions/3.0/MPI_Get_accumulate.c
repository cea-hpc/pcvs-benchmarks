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
    MPI_Aint var_7;
    int var_8;
    MPI_Datatype var_9;
    MPI_Op var_10;
    MPI_Win var_11;
    int ret;
    /* calls */
    ret = MPI_Get_accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    ret = PMPI_Get_accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    return 0;
}
