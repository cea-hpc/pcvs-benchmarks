#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char *var_1;
    int *var_2;
    int *var_3;
    MPI_Datatype *var_4;
    MPI_T_enum *var_5;
    char *var_6;
    int *var_7;
    int *var_8;
    int *var_9;
    int ret;
    /* calls */
    ret = MPI_T_cvar_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    ret = PMPI_T_cvar_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9);
    return 0;
}
