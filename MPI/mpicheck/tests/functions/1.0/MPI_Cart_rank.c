#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    const int var_1[2];
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Cart_rank(var_0, var_1, var_2);
    ret = PMPI_Cart_rank(var_0, var_1, var_2);
    return 0;
}
