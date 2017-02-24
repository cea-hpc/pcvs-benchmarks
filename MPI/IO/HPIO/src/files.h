/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef FILES_H
#define FILES_H

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

int create_output_filenames(char *output_info, 
			    char *output_header, 
			    char *output_min_results,
			    char *output_results,
			    char *output_failed,
			    int max_filename_sz, 
			    struct test_params_s *test_params_p);
int disp_io(MPI_File fh, char *buf, MPI_Offset disp, int rw_type);
int precreate_file(int test_type,
		   int noncontig_type,
		   int io_method_type,
		   int param_val,
		   struct test_params_s *test_params_p);
int alloc_and_fill_io_buffers(
    char *filename, int myid, int numprocs,
    int region_count, int region_size, int region_spacing,
    int noncontig_type, MPI_Datatype *base_dtype_p, 
    MPI_Offset fileview_disp, int rw_type,
    struct test_params_s *test_params_p, 
    char **io_buf_p, int64_t *io_buf_sz_p, 
    char **data_buf_p, int64_t *data_buf_sz_p);
int gen_files(char *filename, int myid, int numprocs,
	      int region_count, int region_size, int region_spacing, 
	      int noncontig_type, MPI_Datatype *base_dtype_p, 
	      MPI_Offset fileview_disp,
	      struct test_params_s *test_params_p);
int generate_files(struct test_params_s *test_params_p);
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
