#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    const int var_2[2];
    const int var_3[2];
    int var_4;
    const int var_5[2];
    const int var_6[2];
    MPI_Info var_7;
    int var_8;
    MPI_Comm *var_9;
    int ret;
    /* calls */
    ret = MPI_Dist_graph_create_adjacent(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_Dist_graph_create_adjacent(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}
