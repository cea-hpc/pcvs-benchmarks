#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Datatype var_2;
    void *var_3;
    const int var_4[2];
    const int var_5[2];
    MPI_Datatype var_6;
    MPI_Comm var_7;
    int ret;
    /* calls */
    ret = MPI_Neighbor_allgatherv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Neighbor_allgatherv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
