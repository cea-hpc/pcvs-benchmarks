#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Datatype var_0;
    MPI_Aint *var_1;
    MPI_Aint *var_2;
    int ret;
    /* calls */
    ret = MPI_Type_get_extent(var_0, var_1, var_2);
    ret = PMPI_Type_get_extent(var_0, var_1, var_2);
    return 0;
}
