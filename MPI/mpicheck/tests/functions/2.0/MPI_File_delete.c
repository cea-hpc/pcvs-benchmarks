#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const char *var_0;
    MPI_Info var_1;
    int ret;
    /* calls */
    ret = MPI_File_delete(var_0, var_1);
    ret = PMPI_File_delete(var_0, var_1);
    return 0;
}
