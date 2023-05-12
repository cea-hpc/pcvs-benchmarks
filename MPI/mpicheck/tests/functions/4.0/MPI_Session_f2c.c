#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Session ret;
    /* calls */
    ret = MPI_Session_f2c(var_0);
    ret = PMPI_Session_f2c(var_0);
    return 0;
}
