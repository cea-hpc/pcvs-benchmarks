#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    const int var_2[2];
    const int var_3[2];
    int var_4;
    MPI_Comm *var_5;
    int ret;
    /* calls */
    ret = MPI_Graph_create(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Graph_create(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
