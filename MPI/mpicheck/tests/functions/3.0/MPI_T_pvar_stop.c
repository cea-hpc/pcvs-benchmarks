#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_pvar_session var_0;
    MPI_T_pvar_handle var_1;
    int ret;
    /* calls */
    ret = MPI_T_pvar_stop(var_0, var_1);
    ret = PMPI_T_pvar_stop(var_0, var_1);
    return 0;
}
