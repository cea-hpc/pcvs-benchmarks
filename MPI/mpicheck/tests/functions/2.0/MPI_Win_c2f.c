#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Win_c2f(var_0);
    ret = PMPI_Win_c2f(var_0);
    return 0;
}
