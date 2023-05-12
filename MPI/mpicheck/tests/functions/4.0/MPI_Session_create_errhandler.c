#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session_errhandler_function *var_0;
    MPI_Errhandler *var_1;
    int ret;
    /* calls */
    ret = MPI_Session_create_errhandler(var_0, var_1);
    ret = PMPI_Session_create_errhandler(var_0, var_1);
    return 0;
}
