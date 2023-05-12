#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    const void *var_1;
    MPI_Status *var_2;
    int ret;
    /* calls */
    ret = MPI_File_write_ordered_end(var_0, var_1, var_2);
    ret = PMPI_File_write_ordered_end(var_0, var_1, var_2);
    return 0;
}
