#include "mpi.h"
#include "defined_func.h"

#define DISP  0
#define COUNT 2
int d_idx_idx_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p)
{
    MPI_Datatype tmp_dtype;
    
    int blk_arr[COUNT] = {1, 2};
    int dsp_arr[COUNT] = {0, 2};
    
    MPI_Type_indexed(COUNT, blk_arr, dsp_arr,
                     MPI_CHAR, &tmp_dtype);
    MPI_Type_commit(&tmp_dtype);
    MPI_Type_indexed(COUNT, blk_arr, dsp_arr,
                     tmp_dtype, dtype_p);
    MPI_Type_commit(dtype_p);
    
    *disp_p = DISP;
    
    MPI_Type_free(&tmp_dtype);
    return 0;
}
#undef DISP
#undef COUNT

