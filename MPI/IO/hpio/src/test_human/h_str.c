#include "mpi.h"
#include "human_func.h"
#include "human.h"

#define DISP  1
#define COUNT 2
int h_str_init(MPI_Datatype *dtype_p,
	       MPI_Offset *disp_p)
{
    int blk_arr[COUNT] = {1, 2};
    MPI_Aint dsp_arr[COUNT] = {0, 4};
    MPI_Datatype dtype_arr[COUNT];

    dtype_arr[0] = MPI_CHAR;
    dtype_arr[1] = MPI_CHAR;
    
    MPI_Type_struct(COUNT, blk_arr, dsp_arr,
		    dtype_arr, dtype_p);
    MPI_Type_commit(dtype_p);
    
    *disp_p = DISP;
    return 0;
}
#undef DISP
#undef COUNT

#define CONTIG_BUF_SZ    3
#define NONCONTIG_BUF_SZ 7
int h_str_alloc_check_buf(int rw_type,
			  int noncontig_type,
			  int **check_buf_p,
			  int64_t *check_buf_sz_p,
			  int64_t *check_buf_data_sz_p,
			  int64_t *initial_skip_p)
{
    int contig_buf[CONTIG_BUF_SZ]    
	= { 0,  1,  2};
    int noncontig_buf[NONCONTIG_BUF_SZ] 
	= {XX,  0, XX, XX, XX,    1,  2};

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
