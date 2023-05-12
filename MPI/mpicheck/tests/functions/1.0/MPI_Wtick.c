#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    double ret;
    /* calls */
    ret = MPI_Wtick();
    ret = PMPI_Wtick();
    return 0;
}
