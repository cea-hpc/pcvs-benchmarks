#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    MPI_Count var_1;
    MPI_Datatype var_2;
    void *var_3;
    MPI_Count var_4;
    MPI_Datatype var_5;
    MPI_Comm var_6;
    int ret;
    /* calls */
    ret = MPI_Alltoall_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Alltoall_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
