#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    MPI_Count var_1;
    MPI_Datatype var_2;
    int var_3;
    int var_4;
    void *var_5;
    MPI_Count var_6;
    MPI_Datatype var_7;
    int var_8;
    int var_9;
    MPI_Comm var_10;
    MPI_Status *var_11;
    int ret;
    /* calls */
    ret = MPI_Sendrecv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    ret = PMPI_Sendrecv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    return 0;
}
