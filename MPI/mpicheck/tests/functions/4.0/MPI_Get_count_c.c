#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_Status *var_0;
    MPI_Datatype var_1;
    MPI_Count *var_2;
    int ret;
    /* calls */
    ret = MPI_Get_count_c(var_0, var_1, var_2);
    ret = PMPI_Get_count_c(var_0, var_1, var_2);
    return 0;
}
