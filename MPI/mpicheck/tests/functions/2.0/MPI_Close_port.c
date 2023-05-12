#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    int ret;
    /* calls */
    ret = MPI_Close_port(var_0);
    ret = PMPI_Close_port(var_0);
    return 0;
}
