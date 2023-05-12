#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Op var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Op_c2f(var_0);
    ret = PMPI_Op_c2f(var_0);
    return 0;
}
