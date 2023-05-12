#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Aint var_0;
    MPI_Info var_1;
    void *var_2;
    int ret;
    /* calls */
    ret = MPI_Alloc_mem(var_0, var_1, var_2);
    ret = PMPI_Alloc_mem(var_0, var_1, var_2);
    return 0;
}
