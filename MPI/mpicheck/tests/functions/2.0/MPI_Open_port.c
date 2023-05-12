#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    char *var_1;
    int ret;
    /* calls */
    ret = MPI_Open_port(var_0, var_1);
    ret = PMPI_Open_port(var_0, var_1);
    return 0;
}
