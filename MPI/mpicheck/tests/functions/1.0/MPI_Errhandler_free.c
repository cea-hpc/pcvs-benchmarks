#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Errhandler *var_0;
    int ret;
    /* calls */
    ret = MPI_Errhandler_free(var_0);
    ret = PMPI_Errhandler_free(var_0);
    return 0;
}
