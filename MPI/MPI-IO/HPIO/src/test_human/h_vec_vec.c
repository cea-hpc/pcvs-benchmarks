#include "mpi.h"
#include "human_func.h"
#include "human.h"

#define DISP  0
#define COUNT 0
int h_vec_vec_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p)
{
    MPI_Datatype tmp_dtype;
    MPI_Type_vector(3, 1, 2, MPI_CHAR, &tmp_dtype);
    MPI_Type_vector(4, 1, 3, tmp_dtype, dtype_p);
    MPI_Type_commit(dtype_p);
    
    *disp_p = DISP;

    MPI_Type_free(&tmp_dtype);
    return 0;
}
#undef DISP
#undef COUNT

#define CONTIG_BUF_SZ    12
#define NONCONTIG_BUF_SZ 50
int h_vec_vec_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p)
{
    int contig_buf[CONTIG_BUF_SZ]    
	= { 0,  1,  2,  3,  4,    5,  6,  7,  8,  9,
	   10, 11};
    int noncontig_buf[NONCONTIG_BUF_SZ] 
	= { 0, XX,  1, XX,  2,   XX, XX, XX, XX, XX,
	   XX, XX, XX, XX, XX,    3, XX,  4, XX,  5,   
	   XX, XX, XX, XX, XX,	 XX, XX, XX, XX, XX,
	    6, XX,  7, XX,  8,   XX, XX, XX, XX, XX,
	   XX, XX, XX, XX, XX,    9, XX, 10, XX, 11};

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
