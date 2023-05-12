#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    void *var_1;
    MPI_Info var_2;
    MPI_T_event_registration *var_3;
    int ret;
    /* calls */
    ret = MPI_T_event_handle_alloc(var_0, var_1, var_2, var_3);
    ret = PMPI_T_event_handle_alloc(var_0, var_1, var_2, var_3);
    return 0;
}
