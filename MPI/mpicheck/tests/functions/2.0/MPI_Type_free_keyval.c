#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    int ret;
    /* calls */
    ret = MPI_Type_free_keyval(var_0);
    ret = PMPI_Type_free_keyval(var_0);
    return 0;
}
