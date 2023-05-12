#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    const int var_1[2];
    const int var_2[2];
    const int var_3[2];
    int var_4;
    MPI_Datatype var_5;
    MPI_Datatype *var_6;
    int ret;
    /* calls */
    ret = MPI_Type_create_subarray(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Type_create_subarray(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
