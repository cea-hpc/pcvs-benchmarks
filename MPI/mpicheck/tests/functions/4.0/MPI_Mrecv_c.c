#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    MPI_Count var_1;
    MPI_Datatype var_2;
    MPI_Message *var_3;
    MPI_Status *var_4;
    int ret;
    /* calls */
    ret = MPI_Mrecv_c(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Mrecv_c(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
