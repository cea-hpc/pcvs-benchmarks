#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    MPI_Group var_1;
    int var_2;
    MPI_Comm *var_3;
    int ret;
    /* calls */
    ret = MPI_Comm_create_group(var_0, var_1, var_2, var_3);
    ret = PMPI_Comm_create_group(var_0, var_1, var_2, var_3);
    return 0;
}
