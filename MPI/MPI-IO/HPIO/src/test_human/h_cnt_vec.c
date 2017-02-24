#include "mpi.h"
#include "human_func.h"
#include "human.h"

#define DISP 0
int h_cnt_vec_init(MPI_Datatype *dtype_p,
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

#define CONTIG_BUF_SZ    6
#define NONCONTIG_BUF_SZ 10
int h_cnt_vec_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p)
{
    int contig_buf[CONTIG_BUF_SZ]    
	= { 0,  1,  2,  3,  4,    5};
    int noncontig_buf[NONCONTIG_BUF_SZ] 
	= { 0, XX,  1, XX,  2,    3, XX,  4, XX,  5};

    *initial_skip_p = 0;

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
