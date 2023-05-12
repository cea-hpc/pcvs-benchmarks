#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_pvar_session var_0;
    MPI_T_pvar_handle var_1;
    const void *var_2;
    int ret;
    /* calls */
    ret = MPI_T_pvar_write(var_0, var_1, var_2);
    ret = PMPI_T_pvar_write(var_0, var_1, var_2);
    return 0;
}
