#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Group_size(var_0, var_1);
    ret = PMPI_Group_size(var_0, var_1);
    return 0;
}
