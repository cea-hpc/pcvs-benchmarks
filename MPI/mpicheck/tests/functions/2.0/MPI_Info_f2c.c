#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Info ret;
    /* calls */
    ret = MPI_Info_f2c(var_0);
    ret = PMPI_Info_f2c(var_0);
    return 0;
}