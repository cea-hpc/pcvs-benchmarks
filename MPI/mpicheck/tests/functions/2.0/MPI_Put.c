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
    MPI_Win var_7;
    int ret;
    /* calls */
    ret = MPI_Put(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Put(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
