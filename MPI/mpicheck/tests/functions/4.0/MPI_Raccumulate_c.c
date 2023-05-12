#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    MPI_Count var_1;
    MPI_Datatype var_2;
    int var_3;
    MPI_Aint var_4;
    MPI_Count var_5;
    MPI_Datatype var_6;
    MPI_Op var_7;
    MPI_Win var_8;
    MPI_Request *var_9;
    int ret;
    /* calls */
    ret = MPI_Raccumulate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_Raccumulate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}
