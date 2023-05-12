#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_pvar_session var_0;
    int var_1;
    void *var_2;
    MPI_T_pvar_handle *var_3;
    int *var_4;
    int ret;
    /* calls */
    ret = MPI_T_pvar_handle_alloc(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_T_pvar_handle_alloc(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
