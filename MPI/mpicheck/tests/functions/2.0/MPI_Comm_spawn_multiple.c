#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char *var_1[2];
    char **var_2[2];
    const int var_3[2];
    const MPI_Info var_4[2];
    int var_5;
    MPI_Comm var_6;
    MPI_Comm *var_7;
    int var_8[2];
    int ret;
    /* calls */
    ret = MPI_Comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
