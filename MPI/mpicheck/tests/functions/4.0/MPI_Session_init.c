#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    MPI_Errhandler var_1;
    MPI_Session *var_2;
    int ret;
    /* calls */
    ret = MPI_Session_init(var_0, var_1, var_2);
    ret = PMPI_Session_init(var_0, var_1, var_2);
    return 0;
}
