#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session var_0;
    const char *var_1;
    MPI_Info *var_2;
    int ret;
    /* calls */
    ret = MPI_Session_get_pset_info(var_0, var_1, var_2);
    ret = PMPI_Session_get_pset_info(var_0, var_1, var_2);
    return 0;
}
