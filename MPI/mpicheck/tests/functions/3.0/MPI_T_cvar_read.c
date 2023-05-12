#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_cvar_handle var_0;
    void *var_1;
    int ret;
    /* calls */
    ret = MPI_T_cvar_read(var_0, var_1);
    ret = PMPI_T_cvar_read(var_0, var_1);
    return 0;
}
