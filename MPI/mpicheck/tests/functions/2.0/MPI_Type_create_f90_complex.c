#include <mpi.h>
int main(char argc, char**argv)
{
    /* vars */
    int var_0;
    int var_1;
    MPI_Datatype *var_2;
    int ret;
    /* calls */
    ret = MPI_Type_create_f90_complex(var_0, var_1, var_2);
    ret = PMPI_Type_create_f90_complex(var_0, var_1, var_2);
    return 0;
}
