#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    char *var_1;
    int *var_2;
    char *var_3;
    int *var_4;
    int *var_5;
    int *var_6;
    int *var_7;
    int ret;
    /* calls */
    ret = MPI_T_category_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    ret = PMPI_T_category_get_info(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7);
    return 0;
}
