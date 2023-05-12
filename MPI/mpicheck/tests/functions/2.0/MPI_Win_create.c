#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    MPI_Aint var_1;
    int var_2;
    MPI_Info var_3;
    MPI_Comm var_4;
    MPI_Win *var_5;
    int ret;
    /* calls */
    ret = MPI_Win_create(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_Win_create(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
