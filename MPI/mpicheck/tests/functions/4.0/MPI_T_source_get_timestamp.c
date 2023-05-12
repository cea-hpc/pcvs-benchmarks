#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Count *var_1;
    int ret;
    /* calls */
    ret = MPI_T_source_get_timestamp(var_0, var_1);
    ret = PMPI_T_source_get_timestamp(var_0, var_1);
    return 0;
}
