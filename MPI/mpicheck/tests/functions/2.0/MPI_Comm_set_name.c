#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    const char *var_1;
    int ret;
    /* calls */
    ret = MPI_Comm_set_name(var_0, var_1);
    ret = PMPI_Comm_set_name(var_0, var_1);
    return 0;
}
