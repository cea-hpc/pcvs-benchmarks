#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    void *var_0;
    int var_1;
    MPI_Datatype var_2;
    MPI_Message *var_3;
    MPI_Request *var_4;
    int ret;
    /* calls */
    ret = MPI_Imrecv(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Imrecv(var_0, var_1, var_2, var_3, var_4);
    return 0;
}