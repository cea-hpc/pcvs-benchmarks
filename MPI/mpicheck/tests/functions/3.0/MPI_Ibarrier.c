#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    MPI_Request *var_1;
    int ret;
    /* calls */
    ret = MPI_Ibarrier(var_0, var_1);
    ret = PMPI_Ibarrier(var_0, var_1);
    return 0;
}
