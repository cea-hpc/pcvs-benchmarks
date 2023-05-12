#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_Session var_0;
    MPI_Info var_1;
    int var_2;
    int *var_3;
    char *var_4;
    int ret;
    /* calls */
    ret = MPI_Session_get_nth_pset(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_Session_get_nth_pset(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
