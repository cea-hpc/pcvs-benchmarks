#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Session_c2f(var_0);
    ret = PMPI_Session_c2f(var_0);
    return 0;
}
