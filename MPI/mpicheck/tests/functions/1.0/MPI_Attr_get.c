#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    void *var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_Attr_get(var_0, var_1, var_2, var_3);
    ret = PMPI_Attr_get(var_0, var_1, var_2, var_3);
    return 0;
}
