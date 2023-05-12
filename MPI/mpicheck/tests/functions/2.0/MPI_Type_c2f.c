#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Type_c2f(var_0);
    ret = PMPI_Type_c2f(var_0);
    return 0;
}
