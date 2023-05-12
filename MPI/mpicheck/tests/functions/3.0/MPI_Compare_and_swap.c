#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    const void *var_1;
    void *var_2;
    MPI_Datatype var_3;
    int var_4;
    MPI_Aint var_5;
    MPI_Win var_6;
    int ret;
    /* calls */
    ret = MPI_Compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
