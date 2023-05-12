#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    double ret;
    /* calls */
    ret = MPI_Wtime();
    ret = PMPI_Wtime();
    return 0;
}
