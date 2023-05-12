#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_Status *var_0;
    MPI_Datatype var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Get_elements(var_0, var_1, var_2);
    ret = PMPI_Get_elements(var_0, var_1, var_2);
    return 0;
}
