#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    const int var_1[2];
    const int var_2[2];
    MPI_Datatype var_3;
    void *var_4;
    const int var_5[2];
    const int var_6[2];
    MPI_Datatype var_7;
    MPI_Comm var_8;
    int ret;
    /* calls */
    ret = MPI_Neighbor_alltoallv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Neighbor_alltoallv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
