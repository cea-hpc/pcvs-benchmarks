#include "mpi.h"
#include "human_func.h"
#include "human.h"

#define DISP  1
#define COUNT 2
int h_idx_off_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p)
{
    int blk_arr[COUNT] = {1, 2};
    int dsp_arr[COUNT] = {2, 5};
    
    MPI_Type_indexed(COUNT, blk_arr, dsp_arr,
                     MPI_CHAR, dtype_p);
    MPI_Type_commit(dtype_p);
    
    *disp_p = DISP;
    return 0;
}
#undef DISP
#undef COUNT

#define CONTIG_BUF_SZ    3
#define NONCONTIG_BUF_SZ 8
int h_idx_off_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p)
{
    int contig_buf[CONTIG_BUF_SZ]    
	= { 0,  1,  2};
    int noncontig_buf[NONCONTIG_BUF_SZ] 
	= {XX, XX, XX,  0, XX,   XX,  1,  2};

    *initial_skip_p = 3;

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
