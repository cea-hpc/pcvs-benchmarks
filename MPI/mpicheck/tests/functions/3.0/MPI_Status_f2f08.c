#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    const MPI_Fint *var_0;
    MPI_F08_status *var_1;
    int ret;
    /* calls */
    ret = MPI_Status_f2f08(var_0, var_1);
    ret = PMPI_Status_f2f08(var_0, var_1);
    return 0;
}