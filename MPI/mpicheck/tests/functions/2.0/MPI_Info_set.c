#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    const char *var_1;
    const char *var_2;
    int ret;
    /* calls */
    ret = MPI_Info_set(var_0, var_1, var_2);
    ret = PMPI_Info_set(var_0, var_1, var_2);
    return 0;
}
