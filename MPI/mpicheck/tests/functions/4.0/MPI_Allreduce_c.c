#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    void *var_1;
    MPI_Count var_2;
    MPI_Datatype var_3;
    MPI_Op var_4;
    MPI_Comm var_5;
    int ret;
    /* calls */
    ret = MPI_Allreduce_c(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Allreduce_c(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
