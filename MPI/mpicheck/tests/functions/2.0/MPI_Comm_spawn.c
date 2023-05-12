#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    char *var_1[2];
    int var_2;
    MPI_Info var_3;
    int var_4;
    MPI_Comm var_5;
    MPI_Comm *var_6;
    int var_7[2];
    int ret;
    /* calls */
    ret = MPI_Comm_spawn(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Comm_spawn(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
