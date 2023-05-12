#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    MPI_Comm var_2;
    MPI_Message *var_3;
    MPI_Status *var_4;
    int ret;
    /* calls */
    ret = MPI_Mprobe(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Mprobe(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
