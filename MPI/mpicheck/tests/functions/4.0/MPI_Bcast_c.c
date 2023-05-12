#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    MPI_Count var_1;
    MPI_Datatype var_2;
    int var_3;
    MPI_Comm var_4;
    int ret;
    /* calls */
    ret = MPI_Bcast_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Bcast_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
