#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Copy_function *var_0;
    MPI_Delete_function *var_1;
    int *var_2;
    void *var_3;
    int ret;
    /* calls */
    ret = MPI_Keyval_create(var_0, var_1, var_2, var_3);
    ret = PMPI_Keyval_create(var_0, var_1, var_2, var_3);
    return 0;
}
