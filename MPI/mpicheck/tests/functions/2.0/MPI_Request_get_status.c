#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Request var_0;
    int *var_1;
    MPI_Status *var_2;
    int ret;
    /* calls */
    ret = MPI_Request_get_status(var_0, var_1, var_2);
    ret = PMPI_Request_get_status(var_0, var_1, var_2);
    return 0;
}
