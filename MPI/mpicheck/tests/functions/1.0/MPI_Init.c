#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    char ***var_1;
    int ret;
    /* calls */
    ret = MPI_Init(var_0, var_1);
    ret = PMPI_Init(var_0, var_1);
    return 0;
}
