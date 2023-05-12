#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Request ret;
    /* calls */
    ret = MPI_Request_f2c(var_0);
    ret = PMPI_Request_f2c(var_0);
    return 0;
}
