#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Group var_0;
    MPI_Fint ret;
    /* calls */
    ret = MPI_Group_c2f(var_0);
    ret = PMPI_Group_c2f(var_0);
    return 0;
}
