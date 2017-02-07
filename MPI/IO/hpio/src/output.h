/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef OUTPUT_H
#define OUTPUT_H

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

int print_header(FILE *stream, struct test_params_s *test_params_p);
int preprint_min_output(FILE *stream,
			int rw_type,
			int test_type,
			int noncontig_type,
			int io_method_type);
int print_min_timing(FILE *stream,
		     int reps_completed, int test_type,
		     int region_count, int region_size, int region_spacing,
		     int param_val, double *final_time_arr, 
		     struct test_params_s *test_params_p);
int print_full_timing_header(FILE *stream);
int print_full_timing(FILE *stream, int test_type,
		      int noncontig_type, int pattern_dtype, int rw_type, 
		      int io_method, int reps_completed, 
		      MPI_Offset general_disp, MPI_Datatype *base_dtype_p,
		      int region_count, int region_size, int region_spacing,
		      int param_val, double *final_time_arr, 
		      struct test_params_s *test_params_p);


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
