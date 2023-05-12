#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    int var_1;
    MPI_Win var_2;
    int ret;
    /* calls */
    ret = MPI_Win_post(var_0, var_1, var_2);
    ret = PMPI_Win_post(var_0, var_1, var_2);
    return 0;
}
