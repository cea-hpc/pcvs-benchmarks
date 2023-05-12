#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Datatype *var_1;
    int ret;
    /* calls */
    ret = MPI_Type_create_f90_integer(var_0, var_1);
    ret = PMPI_Type_create_f90_integer(var_0, var_1);
    return 0;
}
