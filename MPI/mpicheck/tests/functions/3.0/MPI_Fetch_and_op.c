#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    void *var_1;
    MPI_Datatype var_2;
    int var_3;
    MPI_Aint var_4;
    MPI_Op var_5;
    MPI_Win var_6;
    int ret;
    /* calls */
    ret = MPI_Fetch_and_op(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Fetch_and_op(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
