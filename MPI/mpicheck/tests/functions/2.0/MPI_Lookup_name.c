#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    MPI_Info var_1;
    char *var_2;
    int ret;
    /* calls */
    ret = MPI_Lookup_name(var_0, var_1, var_2);
    ret = PMPI_Lookup_name(var_0, var_1, var_2);
    return 0;
}