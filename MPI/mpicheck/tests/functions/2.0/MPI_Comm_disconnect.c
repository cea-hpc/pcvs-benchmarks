#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm *var_0;
    int ret;
    /* calls */
    ret = MPI_Comm_disconnect(var_0);
    ret = PMPI_Comm_disconnect(var_0);
    return 0;
}
