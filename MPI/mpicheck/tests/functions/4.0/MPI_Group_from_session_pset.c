#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session var_0;
    const char *var_1;
    MPI_Group *var_2;
    int ret;
    /* calls */
    ret = MPI_Group_from_session_pset(var_0, var_1, var_2);
    ret = PMPI_Group_from_session_pset(var_0, var_1, var_2);
    return 0;
}
