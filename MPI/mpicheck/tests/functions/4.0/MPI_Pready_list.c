#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    const int var_1[2];
    MPI_Request var_2;
    int ret;
    /* calls */
    ret = MPI_Pready_list(var_0, var_1, var_2);
    ret = PMPI_Pready_list(var_0, var_1, var_2);
    return 0;
}
