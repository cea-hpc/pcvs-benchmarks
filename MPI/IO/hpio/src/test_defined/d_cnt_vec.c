#include "mpi.h"
#include "defined_func.h"

#define DISP 0
int d_cnt_vec_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p)
{
    MPI_Datatype cnt_dtype;

    MPI_Type_vector(3, 1, 2, MPI_CHAR, &cnt_dtype);
    MPI_Type_commit(&cnt_dtype);
    MPI_Type_contiguous(2, cnt_dtype, dtype_p);
    MPI_Type_commit(dtype_p);

    MPI_Type_free(&cnt_dtype);

    *disp_p = DISP;
    return 0;
}
#undef DISP
