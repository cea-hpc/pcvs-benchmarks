/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef TEST_HUMAN_H
#define TEST_HUMAN_H

#define _GNU_SOURCE
#include "mpi.h"
#include <mpi.h>
#include <getopt.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include "../test_params.h"
#include "human_func.h"

/* For the human tests, first we need to create the access pattern
 * with init_func_arr().  Finally check against known buffers with
 * alloc_buf_func_arr(). */

char *human_name[MAX_HUMAN_COUNT] = {"h_cnt_vec", 
				     "h_idx",
				     "h_idx_off",
				     "h_idx_off2",
				     "h_idx_off3",
				     "h_str",
				     "h_str_lb_ub",
				     "h_vec",
				     "h_vec_vec"};

int (*init_human_func_arr[MAX_HUMAN_COUNT])(MPI_Datatype *dtype_p,
					    MPI_Offset *disp_p) =
{
    h_cnt_vec_init,
    h_idx_init,
    h_idx_off_init,
    h_idx_off2_init,
    h_idx_off3_init,
    h_str_init,
    h_str_lb_ub_init,
    h_vec_init,
    h_vec_vec_init
};

int (*alloc_buf_func_arr[MAX_HUMAN_COUNT])(int rw_type,
					   int noncontig_type,
					   int **check_buf_p,
					   int64_t *check_buf_sz_p,
					   int64_t *check_buf_data_sz_p,
					   int64_t *initial_skip_p) =
{
    h_cnt_vec_alloc_check_buf,
    h_idx_alloc_check_buf,
    h_idx_off_alloc_check_buf,
    h_idx_off2_alloc_check_buf,
    h_idx_off3_alloc_check_buf,
    h_str_alloc_check_buf,
    h_str_lb_ub_alloc_check_buf,
    h_vec_alloc_check_buf,
    h_vec_vec_alloc_check_buf
};

int human_cpy_buf(int noncontig_type,
                  char *contig_buf,
                  char *noncontig_buf,
                  int64_t buf_sz,
                  char **check_buf_p,
                  int64_t *check_buf_sz_p);

#endif

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
