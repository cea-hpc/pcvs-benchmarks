#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char *var_1;
    int *var_2;
    char *var_3;
    int *var_4;
    MPI_T_source_order *var_5;
    MPI_Count *var_6;
    MPI_Count *var_7;
    MPI_Info *var_8;
    int ret;
    /* calls */
    ret = MPI_T_source_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_T_source_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
