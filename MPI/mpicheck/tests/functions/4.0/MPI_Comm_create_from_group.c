#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    const char *var_1;
    MPI_Info var_2;
    MPI_Errhandler var_3;
    MPI_Comm *var_4;
    int ret;
    /* calls */
    ret = MPI_Comm_create_from_group(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Comm_create_from_group(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
