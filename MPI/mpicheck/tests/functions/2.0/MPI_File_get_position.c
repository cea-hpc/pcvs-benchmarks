#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Offset *var_1;
    int ret;
    /* calls */
    ret = MPI_File_get_position(var_0, var_1);
    ret = PMPI_File_get_position(var_0, var_1);
    return 0;
}
