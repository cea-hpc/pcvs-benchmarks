#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session var_0;
    MPI_Info var_1;
    int *var_2;
    int ret;
    /* calls */
    ret = MPI_Session_get_num_psets(var_0, var_1, var_2);
    ret = PMPI_Session_get_num_psets(var_0, var_1, var_2);
    return 0;
}
