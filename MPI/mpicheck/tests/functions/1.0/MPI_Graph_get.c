#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    int var_2;
    int var_3[2];
    int var_4[2];
    int ret;
    /* calls */
    ret = MPI_Graph_get(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Graph_get(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
