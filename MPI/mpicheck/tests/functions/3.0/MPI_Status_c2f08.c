#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_Status *var_0;
    MPI_F08_status *var_1;
    int ret;
    /* calls */
    ret = MPI_Status_c2f08(var_0, var_1);
    ret = PMPI_Status_c2f08(var_0, var_1);
    return 0;
}
