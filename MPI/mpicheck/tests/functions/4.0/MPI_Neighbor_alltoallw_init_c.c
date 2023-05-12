#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    const MPI_Count var_1[2];
    const MPI_Aint var_2[2];
    const MPI_Datatype var_3[2];
    void *var_4;
    const MPI_Count var_5[2];
    const MPI_Aint var_6[2];
    const MPI_Datatype var_7[2];
    MPI_Comm var_8;
    MPI_Info var_9;
    MPI_Request *var_10;
    int ret;
    /* calls */
    ret = MPI_Neighbor_alltoallw_init_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10);
    ret = PMPI_Neighbor_alltoallw_init_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10);
    return 0;
}
