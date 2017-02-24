/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef HUMAN_H
#define HUMAN_H

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
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>

int human_cpy_buf(int rw_type,
		  int noncontig_type,
		  int *contig_buf,
		  int64_t contig_buf_sz,
		  int *noncontig_buf,
		  int64_t noncontig_buf_sz,
		  int **check_buf_p,
		  int64_t *check_buf_sz_p);

#endif
