#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win_copy_attr_function *var_0;
    MPI_Win_delete_attr_function *var_1;
    int *var_2;
    void *var_3;
    int ret;
    /* calls */
    ret = MPI_Win_create_keyval(var_0, var_1, var_2, var_3);
    ret = PMPI_Win_create_keyval(var_0, var_1, var_2, var_3);
    return 0;
}
