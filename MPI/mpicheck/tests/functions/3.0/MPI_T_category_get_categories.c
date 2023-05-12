#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    int var_2[2];
    int ret;
    /* calls */
    ret = MPI_T_category_get_categories(var_0, var_1, var_2);
    ret = PMPI_T_category_get_categories(var_0, var_1, var_2);
    return 0;
}
