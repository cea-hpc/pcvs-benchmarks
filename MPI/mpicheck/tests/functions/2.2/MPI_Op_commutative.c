#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Op var_0;
    int *var_1;
    int ret;
    /* calls */
    ret = MPI_Op_commutative(var_0, var_1);
    ret = PMPI_Op_commutative(var_0, var_1);
    return 0;
}
