#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_event_registration var_0;
    MPI_Info *var_1;
    int ret;
    /* calls */
    ret = MPI_T_event_handle_get_info(var_0, var_1);
    ret = PMPI_T_event_handle_get_info(var_0, var_1);
    return 0;
}
