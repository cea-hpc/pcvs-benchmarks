#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Fint var_0;
    MPI_Group ret;
    /* calls */
    ret = MPI_Group_f2c(var_0);
    ret = PMPI_Group_f2c(var_0);
    return 0;
}
