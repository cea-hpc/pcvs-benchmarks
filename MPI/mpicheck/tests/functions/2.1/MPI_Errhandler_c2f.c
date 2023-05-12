#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Errhandler var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Errhandler_c2f(var_0);
    ret = PMPI_Errhandler_c2f(var_0);
    return 0;
}
