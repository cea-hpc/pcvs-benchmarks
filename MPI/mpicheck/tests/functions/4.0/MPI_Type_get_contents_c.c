#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Count var_1;
    MPI_Count var_2;
    MPI_Count var_3;
    MPI_Count var_4;
    int var_5[2];
    MPI_Aint var_6[2];
    MPI_Count var_7[2];
    MPI_Datatype var_8[2];
    int ret;
    /* calls */
    ret = MPI_Type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
