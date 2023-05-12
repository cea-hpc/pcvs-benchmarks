#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group *var_0;
    int ret;
    /* calls */
    ret = MPI_Group_free(var_0);
    ret = PMPI_Group_free(var_0);
    return 0;
}
