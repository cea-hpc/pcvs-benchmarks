#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const void *var_0;
    int var_1;
    MPI_Count var_2;
    MPI_Datatype var_3;
    int var_4;
    int var_5;
    MPI_Comm var_6;
    MPI_Info var_7;
    MPI_Request *var_8;
    int ret;
    /* calls */
    ret = MPI_Psend_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    ret = PMPI_Psend_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8);
    return 0;
}
