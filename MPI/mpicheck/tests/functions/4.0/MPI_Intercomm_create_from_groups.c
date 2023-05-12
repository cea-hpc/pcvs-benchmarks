#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    int var_1;
    MPI_Group var_2;
    int var_3;
    const char *var_4;
    MPI_Info var_5;
    MPI_Errhandler var_6;
    MPI_Comm *var_7;
    int ret;
    /* calls */
    ret = MPI_Intercomm_create_from_groups(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_Intercomm_create_from_groups(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
