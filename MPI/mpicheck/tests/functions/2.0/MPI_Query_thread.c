#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Query_thread(var_0);
    ret = PMPI_Query_thread(var_0);
    return 0;
}
