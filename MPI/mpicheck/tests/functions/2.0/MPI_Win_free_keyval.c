#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Win_free_keyval(var_0);
    ret = PMPI_Win_free_keyval(var_0);
    return 0;
}
