/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef TEST_H
#define TEST_H

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

/* Make the variable names and parameter information global */

extern char *rw_name[MAX_RW];
extern char *test_type_name[MAX_TEST];
extern char *test_param_name[MAX_TEST];
extern char *noncontig_type_name[MAX_NONCONTIG];
extern char *io_method_name[MAX_IO_METHOD];
extern char *pattern_dtype_name[MAX_PATTERN_DTYPE];
extern char *fsync_method_name[MAX_FSYNC_METHOD];
extern char *gen_files_name[MAX_GEN_FILES];
extern char *gen_files_print[MAX_GEN_FILES];
extern char *average_name[MAX_AVE];
extern char *human_name[MAX_HUMAN_COUNT];
extern char *defined_name[MAX_DEFINED_COUNT];
extern char *verify_name[MAX_VERIFY];

extern const int params_count[MAX_TEST];
extern const int region_count_arr[MAX_COUNT_COUNT];
extern const int region_size_arr[MAX_SIZE_COUNT];
extern const int region_spacing_arr[MAX_SPACING_COUNT];
extern int human_arr[MAX_HUMAN_COUNT];
extern const int *params_arr_p[MAX_TEST];

int run_test(int rw_type,
	     int pattern_dtype,
	     int test_type,
	     int noncontig_type,
	     int io_method_type,
	     int use_mode,
	     struct test_params_s *test_params_p);
int generate_filename(char *filename, int max_filename_sz, int pattern_dtype,
		      int test_type, int noncontig_type, int io_method_type,
		      int region_count, int region_size, int region_spacing,
		      int param_val, int rep, 
		      struct test_params_s *test_params_p);
int set_hint_info(int hpio_hint, 
		  struct test_params_s *test_params_p, 
		  MPI_Info *info_p);
int create_test_parameters(int test_type, 
			   int param_val,
			   struct test_params_s *test_params_p,
			   int *region_count_p,
			   int *region_size_p,
			   int *region_spacing_p,
			   MPI_Datatype *base_dtype_p,
			   MPI_Offset *fileview_disp_p);
int create_access_pattern(int pattern_dtype,
			  int myid,
			  int numprocs,
			  int noncontig_type,
			  int region_count,
			  int region_size,
			  int region_spacing,
			  MPI_Datatype *base_dtype_p,
			  int *memtype_count_p,
			  MPI_Datatype *memtype_p,
			  MPI_Datatype *filetype_p,
			  MPI_Offset *fileview_disp_p);
void print_time_arr(double *time_arr);
void print_time_matrix(double **test_time_matrix, int reps);
int test_average(int reps_completed,
		 struct test_params_s *test_params_p,
		 double **test_time_matrix,
		 double *final_time_arr);
int check_data(char *buf, int64_t buf_sz,
	       char *filename,
	       int rw_type,
	       int test_type,
	       int noncontig_type,
	       int io_method_type,
	       int param_val,
	       MPI_Offset fileview_disp,
	       int region_count,
	       int region_size,
	       int region_spacing,
	       MPI_Datatype *base_dtype_p,
	       struct test_params_s *test_params_p);
int check_io_status(MPI_Status *status_p,
		    MPI_Datatype *memtype_p,
		    int mpiio_count,
		    int count,
		    int region_count,
		    int region_size,
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
