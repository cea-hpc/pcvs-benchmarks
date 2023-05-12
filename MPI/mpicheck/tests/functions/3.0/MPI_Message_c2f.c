#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Message var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Message_c2f(var_0);
    ret = PMPI_Message_c2f(var_0);
    return 0;
}
