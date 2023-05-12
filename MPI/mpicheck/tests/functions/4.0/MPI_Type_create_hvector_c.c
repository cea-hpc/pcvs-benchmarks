#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Count var_0;
    MPI_Count var_1;
    MPI_Count var_2;
    MPI_Datatype var_3;
    MPI_Datatype *var_4;
    int ret;
    /* calls */
    ret = MPI_Type_create_hvector_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Type_create_hvector_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
