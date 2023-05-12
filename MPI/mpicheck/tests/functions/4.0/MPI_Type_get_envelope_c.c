#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Count *var_1;
    MPI_Count *var_2;
    MPI_Count *var_3;
    MPI_Count *var_4;
    int *var_5;
    int ret;
    /* calls */
    ret = MPI_Type_get_envelope_c(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Type_get_envelope_c(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
