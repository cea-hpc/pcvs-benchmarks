#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Count var_0;
    MPI_Datatype var_1;
    MPI_Comm var_2;
    MPI_Count *var_3;
    int ret;
    /* calls */
    ret = MPI_Pack_size_c(var_0, var_1, var_2, var_3);
    ret = PMPI_Pack_size_c(var_0, var_1, var_2, var_3);
    return 0;
}
