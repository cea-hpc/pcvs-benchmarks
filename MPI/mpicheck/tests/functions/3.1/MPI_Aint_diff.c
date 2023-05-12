#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Aint var_0;
    MPI_Aint var_1;
    MPI_Aint ret;
    /* calls */
    ret = MPI_Aint_diff(var_0, var_1);
    ret = PMPI_Aint_diff(var_0, var_1);
    return 0;
}
