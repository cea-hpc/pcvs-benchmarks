#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Errhandler ret;
    /* calls */
    ret = MPI_Errhandler_f2c(var_0);
    ret = PMPI_Errhandler_f2c(var_0);
    return 0;
}
