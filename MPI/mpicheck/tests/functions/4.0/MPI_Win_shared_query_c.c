#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Win var_0;
    int var_1;
    MPI_Aint *var_2;
    MPI_Aint *var_3;
    void *var_4;
    int ret;
    /* calls */
    ret = MPI_Win_shared_query_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Win_shared_query_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
