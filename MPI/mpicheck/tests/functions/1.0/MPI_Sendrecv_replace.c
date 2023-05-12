#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    int var_1;
    MPI_Datatype var_2;
    int var_3;
    int var_4;
    int var_5;
    int var_6;
    MPI_Comm var_7;
    MPI_Status *var_8;
    int ret;
    /* calls */
    ret = MPI_Sendrecv_replace(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Sendrecv_replace(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
