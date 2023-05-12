#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Comm ret;
    /* calls */
    ret = MPI_Comm_f2c(var_0);
    ret = PMPI_Comm_f2c(var_0);
    return 0;
}
