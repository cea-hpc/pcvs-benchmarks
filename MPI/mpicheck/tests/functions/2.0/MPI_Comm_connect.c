#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    MPI_Info var_1;
    int var_2;
    MPI_Comm var_3;
    MPI_Comm *var_4;
    int ret;
    /* calls */
    ret = MPI_Comm_connect(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Comm_connect(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
