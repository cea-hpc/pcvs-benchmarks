#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    void *var_1;
    int var_2;
    MPI_Datatype var_3;
    int ret;
    /* calls */
    ret = MPI_File_read_ordered_begin(var_0, var_1, var_2, var_3);
    ret = PMPI_File_read_ordered_begin(var_0, var_1, var_2, var_3);
    return 0;
}
