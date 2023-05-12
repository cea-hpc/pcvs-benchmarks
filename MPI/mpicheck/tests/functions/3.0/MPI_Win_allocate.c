#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Aint var_0;
    int var_1;
    MPI_Info var_2;
    MPI_Comm var_3;
    void *var_4;
    MPI_Win *var_5;
    int ret;
    /* calls */
    ret = MPI_Win_allocate(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Win_allocate(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
