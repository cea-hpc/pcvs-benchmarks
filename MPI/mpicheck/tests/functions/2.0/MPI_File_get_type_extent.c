#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Datatype var_1;
    MPI_Aint *var_2;
    int ret;
    /* calls */
    ret = MPI_File_get_type_extent(var_0, var_1, var_2);
    ret = PMPI_File_get_type_extent(var_0, var_1, var_2);
    return 0;
}
