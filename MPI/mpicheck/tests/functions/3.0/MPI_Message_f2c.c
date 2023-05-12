#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Message ret;
    /* calls */
    ret = MPI_Message_f2c(var_0);
    ret = PMPI_Message_f2c(var_0);
    return 0;
}
