#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    void *var_1;
    MPI_T_cvar_handle *var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_T_cvar_handle_alloc(var_0, var_1, var_2, var_3);
    ret = PMPI_T_cvar_handle_alloc(var_0, var_1, var_2, var_3);
    return 0;
}
