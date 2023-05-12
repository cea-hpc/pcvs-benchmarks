#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Offset var_1;
    const void *var_2;
    int var_3;
    MPI_Datatype var_4;
    int ret;
    /* calls */
    ret = MPI_File_write_at_all_begin(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_File_write_at_all_begin(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
