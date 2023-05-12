#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    MPI_Info *var_1;
    int ret;
    /* calls */
    ret = MPI_Info_dup(var_0, var_1);
    ret = PMPI_Info_dup(var_0, var_1);
    return 0;
}
