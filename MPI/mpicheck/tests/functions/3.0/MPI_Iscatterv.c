#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    const int var_1[2];
    const int var_2[2];
    MPI_Datatype var_3;
    void *var_4;
    int var_5;
    MPI_Datatype var_6;
    int var_7;
    MPI_Comm var_8;
    MPI_Request *var_9;
    int ret;
    /* calls */
    ret = MPI_Iscatterv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_Iscatterv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}
