#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Count var_0;
    MPI_Datatype var_1;
    MPI_Datatype *var_2;
    int ret;
    /* calls */
    ret = MPI_Type_contiguous_c(var_0, var_1, var_2);
    ret = PMPI_Type_contiguous_c(var_0, var_1, var_2);
    return 0;
}
