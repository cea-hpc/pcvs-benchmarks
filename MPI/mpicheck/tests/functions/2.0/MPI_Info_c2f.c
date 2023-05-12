#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Info var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Info_c2f(var_0);
    ret = PMPI_Info_c2f(var_0);
    return 0;
}
