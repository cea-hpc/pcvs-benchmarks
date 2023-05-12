#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm_copy_attr_function *var_0;
    MPI_Comm_delete_attr_function *var_1;
    int *var_2;
    void *var_3;
    int ret;
    /* calls */
    ret = MPI_Comm_create_keyval(var_0, var_1, var_2, var_3);
    ret = PMPI_Comm_create_keyval(var_0, var_1, var_2, var_3);
    return 0;
}
