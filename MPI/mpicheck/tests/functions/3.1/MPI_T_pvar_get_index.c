#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    int var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_T_pvar_get_index(var_0, var_1, var_2);
    ret = PMPI_T_pvar_get_index(var_0, var_1, var_2);
    return 0;
}
