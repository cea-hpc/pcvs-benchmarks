#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    MPI_Request var_1[2];
    MPI_Status var_2[2];
    int ret;
    /* calls */
    ret = MPI_Waitall(var_0, var_1, var_2);
    ret = PMPI_Waitall(var_0, var_1, var_2);
    return 0;
}
