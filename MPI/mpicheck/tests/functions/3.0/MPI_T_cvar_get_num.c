#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_T_cvar_get_num(var_0);
    ret = PMPI_T_cvar_get_num(var_0);
    return 0;
}
