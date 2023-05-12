#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    const char *var_1;
    int *var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_Info_get_valuelen(var_0, var_1, var_2, var_3);
    ret = PMPI_Info_get_valuelen(var_0, var_1, var_2, var_3);
    return 0;
}
