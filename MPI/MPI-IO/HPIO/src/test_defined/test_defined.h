/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef TEST_DEFINED_H
#define TEST_DEFINED_H

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
#include "defined_func.h"

/* For the defined tests, first we need to init the buffer to the proper
 * size.  Then we create the access pattern.  Finally check against
 * known buffers. */

char *defined_name[MAX_DEFINED_COUNT] = {"d_dup",
					 "d_idx_idx"};

int (*init_defined_func_arr[MAX_DEFINED_COUNT])(MPI_Datatype *dtype_p,
						MPI_Offset *disp_p) =
{
    d_cnt_vec_init,
    d_idx_idx_init
};

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
