#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session *var_0;
    int ret;
    /* calls */
    ret = MPI_Session_finalize(var_0);
    ret = PMPI_Session_finalize(var_0);
    return 0;
}
