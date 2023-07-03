#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_User_function_c *var_0;
    int var_1;
    MPI_Op *var_2;
    int ret;
    /* calls */
    ret = MPI_Op_create_c(var_0, var_1, var_2);
    ret = PMPI_Op_create_c(var_0, var_1, var_2);
    return 0;
}
