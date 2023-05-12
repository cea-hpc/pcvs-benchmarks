#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char var_0[2];
    const void *var_1;
    int var_2;
    MPI_Datatype var_3;
    void *var_4;
    MPI_Aint var_5;
    MPI_Aint *var_6;
    int ret;
    /* calls */
    ret = MPI_Pack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Pack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
