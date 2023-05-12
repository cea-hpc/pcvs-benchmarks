#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request *var_0;
    int ret;
    /* calls */
    ret = MPI_Start(var_0);
    ret = PMPI_Start(var_0);
    return 0;
}
