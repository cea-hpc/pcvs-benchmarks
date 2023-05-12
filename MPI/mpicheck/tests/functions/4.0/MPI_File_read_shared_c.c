#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    void *var_1;
    MPI_Count var_2;
    MPI_Datatype var_3;
    MPI_Status *var_4;
    int ret;
    /* calls */
    ret = MPI_File_read_shared_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_File_read_shared_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
