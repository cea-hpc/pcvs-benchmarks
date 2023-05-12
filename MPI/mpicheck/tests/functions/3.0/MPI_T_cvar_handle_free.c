#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_cvar_handle *var_0;
    int ret;
    /* calls */
    ret = MPI_T_cvar_handle_free(var_0);
    ret = PMPI_T_cvar_handle_free(var_0);
    return 0;
}
