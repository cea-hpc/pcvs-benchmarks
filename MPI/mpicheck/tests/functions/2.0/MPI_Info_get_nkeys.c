#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Info_get_nkeys(var_0, var_1);
    ret = PMPI_Info_get_nkeys(var_0, var_1);
    return 0;
}
