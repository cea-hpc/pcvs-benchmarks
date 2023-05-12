#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    int var_1;
    int ret;
    /* calls */
    ret = MPI_File_call_errhandler(var_0, var_1);
    ret = PMPI_File_call_errhandler(var_0, var_1);
    return 0;
}
