#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    MPI_Comm var_2;
    MPI_Status *var_3;
    int ret;
    /* calls */
    ret = MPI_Probe(var_0, var_1, var_2, var_3);
    ret = PMPI_Probe(var_0, var_1, var_2, var_3);
    return 0;
}
