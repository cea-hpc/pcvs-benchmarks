#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Count *var_1;
    int ret;
    /* calls */
    ret = MPI_Type_size_c(var_0, var_1);
    ret = PMPI_Type_size_c(var_0, var_1);
    return 0;
}
