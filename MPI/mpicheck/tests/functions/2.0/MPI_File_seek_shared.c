#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Offset var_1;
    int var_2;
    int ret;
    /* calls */
    ret = MPI_File_seek_shared(var_0, var_1, var_2);
    ret = PMPI_File_seek_shared(var_0, var_1, var_2);
    return 0;
}
