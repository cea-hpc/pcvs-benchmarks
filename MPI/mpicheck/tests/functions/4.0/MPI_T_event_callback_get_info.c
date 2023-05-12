#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_event_registration var_0;
    MPI_T_cb_safety var_1;
    MPI_Info *var_2;
    int ret;
    /* calls */
    ret = MPI_T_event_callback_get_info(var_0, var_1, var_2);
    ret = PMPI_T_event_callback_get_info(var_0, var_1, var_2);
    return 0;
}
