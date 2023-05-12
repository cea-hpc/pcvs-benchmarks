#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Op ret;
    /* calls */
    ret = MPI_Op_f2c(var_0);
    ret = PMPI_Op_f2c(var_0);
    return 0;
}
