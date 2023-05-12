#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char *var_1;
    int *var_2;
    int *var_3;
    MPI_Datatype var_4[2];
    MPI_Aint var_5[2];
    int *var_6;
    MPI_T_enum *var_7;
    MPI_Info *var_8;
    char *var_9;
    int *var_10;
    int *var_11;
    int ret;
    /* calls */
    ret = MPI_T_event_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    ret = PMPI_T_event_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11);
    return 0;
}
