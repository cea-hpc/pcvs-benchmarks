#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Comm var_0;
    int var_1;
    void *var_2;
    int ret;
    /* calls */
    ret = MPI_Comm_set_attr(var_0, var_1, var_2);
    ret = PMPI_Comm_set_attr(var_0, var_1, var_2);
    return 0;
}
