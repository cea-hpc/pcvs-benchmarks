#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_pvar_session *var_0;
    int ret;
    /* calls */
    ret = MPI_T_pvar_session_create(var_0);
    ret = PMPI_T_pvar_session_create(var_0);
    return 0;
}
