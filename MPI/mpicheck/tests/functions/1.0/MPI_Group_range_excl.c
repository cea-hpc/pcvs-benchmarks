#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    int var_1;
    int var_2[2][3];
    MPI_Group *var_3;
    int ret;
    /* calls */
    ret = MPI_Group_range_excl(var_0, var_1, var_2, var_3);
    ret = PMPI_Group_range_excl(var_0, var_1, var_2, var_3);
    return 0;
}
