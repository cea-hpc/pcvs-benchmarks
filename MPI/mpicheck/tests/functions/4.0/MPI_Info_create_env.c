#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char var_1[2];
    MPI_Info *var_2;
    int ret;
    /* calls */
    ret = MPI_Info_create_env(var_0, var_1, var_2);
    ret = PMPI_Info_create_env(var_0, var_1, var_2);
    return 0;
}
