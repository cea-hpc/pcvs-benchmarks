#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_File_c2f(var_0);
    ret = PMPI_File_c2f(var_0);
    return 0;
}
