#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    int ret;
    /* calls */
    ret = MPI_Attr_delete(var_0, var_1);
    ret = PMPI_Attr_delete(var_0, var_1);
    return 0;
}
