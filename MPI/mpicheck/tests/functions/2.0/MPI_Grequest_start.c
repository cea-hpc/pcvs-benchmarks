#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Grequest_query_function *var_0;
    MPI_Grequest_free_function *var_1;
    MPI_Grequest_cancel_function *var_2;
    void *var_3;
    MPI_Request *var_4;
    int ret;
    /* calls */
    ret = MPI_Grequest_start(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Grequest_start(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
