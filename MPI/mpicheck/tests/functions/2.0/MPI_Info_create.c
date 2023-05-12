#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info *var_0;
    int ret;
    /* calls */
    ret = MPI_Info_create(var_0);
    ret = PMPI_Info_create(var_0);
    return 0;
}
