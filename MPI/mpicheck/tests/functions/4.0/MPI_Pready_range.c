#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    MPI_Request var_2;
    int ret;
    /* calls */
    ret = MPI_Pready_range(var_0, var_1, var_2);
    ret = PMPI_Pready_range(var_0, var_1, var_2);
    return 0;
}
