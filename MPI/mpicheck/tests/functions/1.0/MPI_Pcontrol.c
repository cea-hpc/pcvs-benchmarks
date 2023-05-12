#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const int var_0;
    char* var_1[10];
    int ret;
    /* calls */
    ret = MPI_Pcontrol(var_0, var_1);
    ret = PMPI_Pcontrol(var_0, var_1);
    return 0;
}
