#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    MPI_T_event_registration var_0;
    MPI_T_cb_safety var_1;
    MPI_Info var_2;
    void *var_3;
    MPI_T_event_cb_function var_4;
    int ret;
    /* calls */
    ret = MPI_T_event_register_callback(var_0, var_1, var_2, var_3, var_4);
    ret = PMPI_T_event_register_callback(var_0, var_1, var_2, var_3, var_4);
    return 0;
}
