#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int *var_0;
    char ***var_1;
    int var_2;
    int *var_3;
    int ret;
    /* calls */
    ret = MPI_Init_thread(var_0, var_1, var_2, var_3);
    ret = PMPI_Init_thread(var_0, var_1, var_2, var_3);
    return 0;
}
