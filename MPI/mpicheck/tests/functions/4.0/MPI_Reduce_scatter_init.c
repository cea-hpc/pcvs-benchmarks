#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    void *var_1;
    const int var_2[2];
    MPI_Datatype var_3;
    MPI_Op var_4;
    MPI_Comm var_5;
    MPI_Info var_6;
    MPI_Request *var_7;
    int ret;
    /* calls */
    ret = MPI_Reduce_scatter_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Reduce_scatter_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
