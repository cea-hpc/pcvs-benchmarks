#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    int var_2[2];
    int var_3[2];
    int var_4;
    int var_5[2];
    int var_6[2];
    int ret;
    /* calls */
    ret = MPI_Dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    ret = PMPI_Dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6);
    return 0;
}
