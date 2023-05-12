#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    void *var_1;
    MPI_Aint var_2;
    int ret;
    /* calls */
    ret = MPI_Win_attach(var_0, var_1, var_2);
    ret = PMPI_Win_attach(var_0, var_1, var_2);
    return 0;
}
