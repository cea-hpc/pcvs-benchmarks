#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_enum var_0;
    int var_1;
    int *var_2;
    char *var_3;
    int *var_4;
    int ret;
    /* calls */
    ret = MPI_T_enum_get_item(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_T_enum_get_item(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
