#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype *var_0;
    int ret;
    /* calls */
    ret = MPI_Type_commit(var_0);
    ret = PMPI_Type_commit(var_0);
    return 0;
}
