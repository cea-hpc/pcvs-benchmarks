#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    int ret;
    /* calls */
    ret = MPI_File_sync(var_0);
    ret = PMPI_File_sync(var_0);
    return 0;
}
