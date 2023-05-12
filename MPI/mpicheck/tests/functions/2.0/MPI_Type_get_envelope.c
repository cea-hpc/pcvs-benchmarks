#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    int *var_1;
    int *var_2;
    int *var_4;
    int *var_5;
    int ret;
    /* calls */
    ret = MPI_Type_get_envelope(var_0, var_1, var_2, var_4, var_5);
    ret = PMPI_Type_get_envelope(var_0, var_1, var_2, var_4, var_5);
    return 0;
}
