#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Status *var_0;
    MPI_Datatype var_1;
    MPI_Count var_2;
    int ret;
    /* calls */
    ret = MPI_Status_set_elements_x(var_0, var_1, var_2);
    ret = PMPI_Status_set_elements_x(var_0, var_1, var_2);
    return 0;
}
