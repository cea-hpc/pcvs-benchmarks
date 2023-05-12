#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Type_copy_attr_function *var_0;
    MPI_Type_delete_attr_function *var_1;
    int *var_2;
    void *var_3;
    int ret;
    /* calls */
    ret = MPI_Type_create_keyval(var_0, var_1, var_2, var_3);
    ret = PMPI_Type_create_keyval(var_0, var_1, var_2, var_3);
    return 0;
}
