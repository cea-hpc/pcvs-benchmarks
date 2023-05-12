#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Request_c2f(var_0);
    ret = PMPI_Request_c2f(var_0);
    return 0;
}
