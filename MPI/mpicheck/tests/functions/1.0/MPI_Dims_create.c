#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    int var_2[2];
    int ret;
    /* calls */
    ret = MPI_Dims_create(var_0, var_1, var_2);
    ret = PMPI_Dims_create(var_0, var_1, var_2);
    return 0;
}
