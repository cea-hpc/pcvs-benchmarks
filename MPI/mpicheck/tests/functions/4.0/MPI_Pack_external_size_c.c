#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char var_0[2];
    MPI_Count var_1;
    MPI_Datatype var_2;
    MPI_Count *var_3;
    int ret;
    /* calls */
    ret = MPI_Pack_external_size_c(var_0, var_1, var_2, var_3);
    ret = PMPI_Pack_external_size_c(var_0, var_1, var_2, var_3);
    return 0;
}
