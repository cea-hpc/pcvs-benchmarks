#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    MPI_Group var_1;
    MPI_Group *var_2;
    int ret;
    /* calls */
    ret = MPI_Group_union(var_0, var_1, var_2);
    ret = PMPI_Group_union(var_0, var_1, var_2);
    return 0;
}
