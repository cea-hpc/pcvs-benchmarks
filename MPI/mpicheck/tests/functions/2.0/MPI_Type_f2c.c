#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Datatype ret;
    /* calls */
    ret = MPI_Type_f2c(var_0);
    ret = PMPI_Type_f2c(var_0);
    return 0;
}
