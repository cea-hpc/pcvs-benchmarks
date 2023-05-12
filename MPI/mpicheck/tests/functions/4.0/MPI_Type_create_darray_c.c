#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    int var_2;
    const MPI_Count var_3[2];
    const int var_4[2];
    const int var_5[2];
    const int var_6[2];
    int var_7;
    MPI_Datatype var_8;
    MPI_Datatype *var_9;
    int ret;
    /* calls */
    ret = MPI_Type_create_darray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_Type_create_darray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}
