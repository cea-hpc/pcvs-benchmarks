#include "mpi.h"
#include "human_func.h"
#include "human.h"

#define DISP  1
#define COUNT 3
int h_str_lb_ub_init(MPI_Datatype *dtype_p,
		     MPI_Offset *disp_p)
{
    int blk_arr[COUNT] = {1, 3, 1};
    MPI_Aint dsp_arr[COUNT] = {0, 4, 10};
    MPI_Datatype dtype_arr[COUNT], tmp_dtype;

    dtype_arr[0] = MPI_LB;
    dtype_arr[1] = MPI_CHAR;
    dtype_arr[2] = MPI_UB;
    
    MPI_Type_struct(COUNT, blk_arr, dsp_arr,
		    dtype_arr, &tmp_dtype);
    MPI_Type_commit(&tmp_dtype);

    MPI_Type_contiguous(2, tmp_dtype, dtype_p);
    MPI_Type_commit(dtype_p);

    *disp_p = DISP;

    MPI_Type_free(&tmp_dtype);
    return 0;
}
#undef DISP
#undef COUNT

#define CONTIG_BUF_SZ    6
#define NONCONTIG_BUF_SZ 21
int h_str_lb_ub_alloc_check_buf(int rw_type,
				int noncontig_type,
				int **check_buf_p,
				int64_t *check_buf_sz_p,
				int64_t *check_buf_data_sz_p,
				int64_t *initial_skip_p)
{
    int contig_buf[CONTIG_BUF_SZ]    
	= { 0,  1,  2,  3,  4,    5};
    int noncontig_buf[NONCONTIG_BUF_SZ] 
	= {XX, XX, XX, XX, XX,    0,  1,  2, XX, XX,
	   XX, XX, XX, XX, XX,    3,  4,  5, XX, XX,
	   XX};

    *initial_skip_p = 1;

    human_cpy_buf(rw_type,
		  noncontig_type,
		  contig_buf,
		  CONTIG_BUF_SZ,
                  &noncontig_buf[*initial_skip_p],
		  NONCONTIG_BUF_SZ - *initial_skip_p,
                  check_buf_p,
                  check_buf_sz_p);

    *check_buf_data_sz_p = CONTIG_BUF_SZ;

    return 0;
}
#undef CONTIG_BUF_SZ
#undef NONCONTIG_BUF_SZ 
