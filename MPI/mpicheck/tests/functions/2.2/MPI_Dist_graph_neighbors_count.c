#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int *var_1;
    int *var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_Dist_graph_neighbors_count(var_0, var_1, var_2, var_3);
    ret = PMPI_Dist_graph_neighbors_count(var_0, var_1, var_2, var_3);
    return 0;
}
