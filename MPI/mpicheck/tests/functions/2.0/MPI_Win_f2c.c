#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Win ret;
    /* calls */
    ret = MPI_Win_f2c(var_0);
    ret = PMPI_Win_f2c(var_0);
    return 0;
}
