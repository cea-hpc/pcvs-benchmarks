/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef BUF_H
#define BUF_H

#ifdef __STRICT_ANSI__
#define inline __inline__
#endif
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
#include "test_params.h"

static inline char gen_char(int64_t byte_num, int mod, int initial_char)
{
    return (char) ((byte_num % mod) + initial_char);
}

static inline char gen_file_char(int64_t byte_num)
{
#if 0
    /* Use letters A-Z */
    return gen_char(byte_num, 26, 65);
#endif
#if 0 
    /* Use numbers 0 - 9 */
    return gen_char(byte_num, 10, 48);
#endif
    if (byte_num % 20 < 10)
	return gen_char(byte_num, 10, 48); /* 0-9 */
    else
	return gen_char(byte_num, 10, 65); /* A-J */
}

int init_data_buf(char *buf, int64_t buf_sz, int myid, int numprocs,
		  int noncontig_type, int region_count, 
		  int region_size, int region_spacing, 
		  int base_dtype_sz, int mpiio_count, 
		  MPI_Datatype *memtype_p);
void init_global_buf(char *buf, int64_t len, int myid, int numprocs,
		     int noncontig_type, int region_count, 
		     int region_size, int region_spacing);
void init_buf(char *buf, int64_t len, int rw_type, int64_t start_byte);
void print_char_buf(char *buf_name, char *buf, int64_t buf_len);
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
