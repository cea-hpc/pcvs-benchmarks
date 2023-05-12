#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Aint var_1;
    MPI_Aint var_2;
    MPI_Datatype *var_3;
    int ret;
    /* calls */
    ret = MPI_Type_create_resized(var_0, var_1, var_2, var_3);
    ret = PMPI_Type_create_resized(var_0, var_1, var_2, var_3);
    return 0;
}
