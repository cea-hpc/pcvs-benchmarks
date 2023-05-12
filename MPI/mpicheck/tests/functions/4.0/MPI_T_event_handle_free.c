#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_event_registration var_0;
    void *var_1;
    MPI_T_event_free_cb_function var_2;
    int ret;
    /* calls */
    ret = MPI_T_event_handle_free(var_0, var_1, var_2);
    ret = PMPI_T_event_handle_free(var_0, var_1, var_2);
    return 0;
}
