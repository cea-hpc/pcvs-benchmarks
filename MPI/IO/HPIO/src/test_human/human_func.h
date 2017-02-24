/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef HUMAN_FUNC_H
#define HUMAN_FUNC_H

#include <sys/types.h>
#include "mpi.h"

#define XX -1

int h_cnt_vec_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p);
int h_idx_init(MPI_Datatype *dtype_p,
	       MPI_Offset *disp_p);
int h_idx_off_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p);
int h_idx_off2_init(MPI_Datatype *dtype_p,
		    MPI_Offset *disp_p);
int h_idx_off3_init(MPI_Datatype *dtype_p,
		    MPI_Offset *disp_p);
int h_str_init(MPI_Datatype *dtype_p,
	       MPI_Offset *disp_p);
int h_str_lb_ub_init(MPI_Datatype *dtype_p,
		     MPI_Offset *disp_p);
int h_vec_init(MPI_Datatype *dtype_p,
	       MPI_Offset *disp_p);
int h_vec_vec_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p);

int h_cnt_vec_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p);
int h_idx_alloc_check_buf(int rw_type,
			  int noncontig_type,
			  int **check_buf_p,
			  int64_t *check_buf_sz_p,
			  int64_t *check_buf_data_sz_p,
			  int64_t *initial_skip_p);
int h_idx_off_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p);
int h_idx_off2_alloc_check_buf(int rw_type,
			       int noncontig_type,
			       int **check_buf_p,
			       int64_t *check_buf_sz_p,
			       int64_t *check_buf_data_sz_p,
			       int64_t *initial_skip_p);
int h_idx_off3_alloc_check_buf(int rw_type,
			       int noncontig_type,
			       int **check_buf_p,
			       int64_t *check_buf_sz_p,
			       int64_t *check_buf_data_sz_p,
			       int64_t *initial_skip_p);
int h_str_alloc_check_buf(int rw_type,
			  int noncontig_type,
			  int **check_buf_p,
			  int64_t *check_buf_sz_p,
			  int64_t *check_buf_data_sz_p,
			  int64_t *initial_skip_p);
int h_str_lb_ub_alloc_check_buf(int rw_type,
				int noncontig_type,
				int **check_buf_p,
				int64_t *check_buf_sz_p,
				int64_t *check_buf_data_sz_p,
				int64_t *initial_skip_p);
int h_vec_alloc_check_buf(int rw_type,
			  int noncontig_type,
			  int **check_buf_p,
			  int64_t *check_buf_sz_p,
			  int64_t *check_buf_data_sz_p,
			  int64_t *initial_skip_p);
int h_vec_vec_alloc_check_buf(int rw_type,
			      int noncontig_type,
			      int **check_buf_p,
			      int64_t *check_buf_sz_p,
			      int64_t *check_buf_data_sz_p,
			      int64_t *initial_skip_p);

#endif
