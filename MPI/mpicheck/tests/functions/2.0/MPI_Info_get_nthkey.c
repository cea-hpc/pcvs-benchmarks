#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    int var_1;
    char *var_2;
    int ret;
    /* calls */
    ret = MPI_Info_get_nthkey(var_0, var_1, var_2);
    ret = PMPI_Info_get_nthkey(var_0, var_1, var_2);
    return 0;
}
