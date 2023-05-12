#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    char *var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Comm_get_name(var_0, var_1, var_2);
    ret = PMPI_Comm_get_name(var_0, var_1, var_2);
    return 0;
}
