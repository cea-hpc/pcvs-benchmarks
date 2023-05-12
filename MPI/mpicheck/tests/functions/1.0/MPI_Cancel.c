#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request *var_0;
    int ret;
    /* calls */
    ret = MPI_Cancel(var_0);
    ret = PMPI_Cancel(var_0);
    return 0;
}
