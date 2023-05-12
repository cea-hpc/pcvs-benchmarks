#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Request var_1;
    int ret;
    /* calls */
    ret = MPI_Pready(var_0, var_1);
    ret = PMPI_Pready(var_0, var_1);
    return 0;
}
