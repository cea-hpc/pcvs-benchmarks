#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    const int var_1[2];
    const int var_2[2];
    const MPI_Datatype var_3[2];
    void *var_4;
    const int var_5[2];
    const int var_6[2];
    const MPI_Datatype var_7[2];
    MPI_Comm var_8;
    MPI_Request *var_9;
    int ret;
    /* calls */
    ret = MPI_Ialltoallw(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_Ialltoallw(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}