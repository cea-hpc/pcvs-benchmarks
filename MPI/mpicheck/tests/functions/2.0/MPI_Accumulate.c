#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Datatype var_2;
    int var_3;
    MPI_Aint var_4;
    int var_5;
    MPI_Datatype var_6;
    MPI_Op var_7;
    MPI_Win var_8;
    int ret;
    /* calls */
    ret = MPI_Accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
