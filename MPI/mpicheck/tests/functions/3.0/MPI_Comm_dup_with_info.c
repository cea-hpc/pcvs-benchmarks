#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    MPI_Info var_1;
    MPI_Comm *var_2;
    int ret;
    /* calls */
    ret = MPI_Comm_dup_with_info(var_0, var_1, var_2);
    ret = PMPI_Comm_dup_with_info(var_0, var_1, var_2);
    return 0;
}
