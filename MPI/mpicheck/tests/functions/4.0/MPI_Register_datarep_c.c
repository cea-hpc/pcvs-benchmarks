#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    MPI_Datarep_conversion_function_c *var_1;
    MPI_Datarep_conversion_function_c *var_2;
    MPI_Datarep_extent_function *var_3;
    void *var_4;
    int ret;
    /* calls */
    ret = MPI_Register_datarep_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Register_datarep_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
