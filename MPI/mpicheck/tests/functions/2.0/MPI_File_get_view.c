#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Offset *var_1;
    MPI_Datatype *var_2;
    MPI_Datatype *var_3;
    char *var_4;
    int ret;
    /* calls */
    ret = MPI_File_get_view(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_File_get_view(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
