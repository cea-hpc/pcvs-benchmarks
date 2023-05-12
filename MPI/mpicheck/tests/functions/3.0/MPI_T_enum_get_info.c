#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_enum var_0;
    int *var_1;
    char *var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_T_enum_get_info(var_0, var_1, var_2, var_3);
    ret = PMPI_T_enum_get_info(var_0, var_1, var_2, var_3);
    return 0;
}
