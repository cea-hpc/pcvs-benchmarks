#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Is_thread_main(var_0);
    ret = PMPI_Is_thread_main(var_0);
    return 0;
}
