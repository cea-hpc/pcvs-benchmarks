#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    MPI_Group var_1;
    MPI_Comm *var_2;
    int ret;
    /* calls */
    ret = MPI_Comm_create(var_0, var_1, var_2);
    ret = PMPI_Comm_create(var_0, var_1, var_2);
    return 0;
}
