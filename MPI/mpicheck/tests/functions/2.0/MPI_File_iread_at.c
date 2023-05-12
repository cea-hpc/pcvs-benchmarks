#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_File var_0;
    MPI_Offset var_1;
    void *var_2;
    int var_3;
    MPI_Datatype var_4;
    MPI_Request *var_5;
    int ret;
    /* calls */
    ret = MPI_File_iread_at(var_0, var_1, var_2, var_3, var_4, var_5);
    ret = PMPI_File_iread_at(var_0, var_1, var_2, var_3, var_4, var_5);
    return 0;
}
