#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_File ret;
    /* calls */
    ret = MPI_File_f2c(var_0);
    ret = PMPI_File_f2c(var_0);
    return 0;
}
