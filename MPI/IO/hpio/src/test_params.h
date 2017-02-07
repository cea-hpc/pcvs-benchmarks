/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef TEST_PARAMS_H
#define TEST_PARAMS_H

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

#ifdef __KERNEL__
#include <linux/types.h>
#else
#include <stdint.h>
#endif

#include "debug.h"

extern int debug_mode;
extern int debug_mask;

#define DEFAULT_CHAR 95      /* DEFAULT_CHAR is the '_' char */
#define DEFAULT_DISP_CHAR 126      /* DEFAULT_DISP_CHAR is the '~' char */
#define DEFAULT_INIT_CHAR 35 /* DEFAULT_INIT_CHAR is the '#' char */
#define DEFAULT_EMPTY_CHAR 0 /* DEFAULT_EMPTY_CHAR is the (null) char */
#define DEFAULT_DATASIEVE_CHAR -1 /* DEFAULT_EMPTY_CHAR is the -1 char */

#define DEFAULT_PARAM_VAL -1 /* No parameter should ever be -1 */
#define FALSE 0
#define TRUE  1

#define HINT_NUM_MAX        1*1024*1024*1024 /* 1 GBytes of integers */
#define HINT_STR_MAX        10   /* Enough to hold 1 GBytes of MAX_INT */
#define MAX_FILENAME_SIZE 1024

#ifndef HPIO_SMALL
#define MAX_COUNT_COUNT   2
#define MAX_SIZE_COUNT    5
#define MAX_SPACING_COUNT 5
#else
#define MAX_COUNT_COUNT   5
#define MAX_SIZE_COUNT    5
#define MAX_SPACING_COUNT 5
#endif

#define MAX_HUMAN_COUNT        9
#define MAX_DEFINED_COUNT      2
#define MAX_BAND_SINGLE_COUNT  1
#define MAX_CHECK_SINGLE_COUNT 1

#define WRITE  0
#define READ   1
#define MAX_RW 2

#define REGION_COUNT   0
#define REGION_SIZE    1
#define REGION_SPACING 2
#define BAND_SINGLE    3
#define HUMAN          4
#define DEFINED        5
#define CHECK_SINGLE   6
#define MAX_TEST       7

/* To do: add selected and random tests. */

#define C_C   0
#define NC_C  1
#define C_NC  2
#define NC_NC 3
#define MAX_NONCONTIG 4

#define INDIVIDUAL    0
#define COLLECTIVE    1
#define MAX_IO_METHOD 2

#define VECTOR            0
#define STRUCT            1
#define MAX_PATTERN_DTYPE 2

#define VAR_RW            0 
#define VAR_TEST          1
#define VAR_NONCONTIG     2
#define VAR_IO_METHOD     3
#define VAR_PARAM_IDX     4
#define MAX_TEST_VARS     5

#define INDIVIDUAL       0
#define COLLECTIVE       1
#define MAX_FSYNC_METHOD 2

#define GENERATE_MODE 0
#define TEST_MODE     1
#define MAX_USE_MODE  2

#define GEN_FULL      0
#define GEN_PARTIAL   1
#define GEN_NONE      2
#define MAX_GEN_FILES 3

#define VERIFY_NONE 0
#define VERIFY_DATA 1
#define VERIFY_FILE 2
#define MAX_VERIFY  3

#define TIME_OPEN           0 
#define TIME_IO             1
#define TIME_SYNC           2
#define TIME_CLOSE          3
#define TIME_TOTAL          4
#define TIME_AGG_MAX_IOSYNC 5 /* Max I/O time for averaging all processes */
#define MAX_TIME            6

#define AVE_NORMAL      0
#define AVE_NOMIN_NOMAX 1
#define AVE_BEST        2
#define MAX_AVE         3

struct test_params_s 
{
    int myid;                       /* My process ID */
    int numprocs;                   /* Number of processes */
    MPI_File fh;
    MPI_Info *info_p;
    char filename_prefix[MAX_FILENAME_SIZE];
    int dir_len;
    char *dir;                      /* Directory to test in*/
    int output_dir_len;
    char *output_dir;               /* Directory for performance results */
    FILE *output_info;
    FILE *output_header;
    FILE *output_min_results;
    FILE *output_results;
    FILE *output_failed;
    int def_region_params[MAX_TEST];/* Default parameters for tests */
    char * def_check_test;
    int def_check_test_len;
    int def_check_test_num;        /* Which check test? */
    int def_check_test_param_val;  /* Which param for the check test? */
    int reps;                       /* Number of repetitions */
    double rep_max_time;            /* If we surpass this time, only 1 rep */
    int average_method;
    int rw[MAX_RW];
    int test[MAX_TEST];
    int noncontig[MAX_NONCONTIG];
    int io_method[MAX_IO_METHOD];
    int enable_fsync;
    int fsync_method;               /* Are we doing the smart fsync? */
    int pattern_dtype;              /* Which dtypes to use for MPI */
    int verify;
    int enable_resume;
    int resume_arr[MAX_TEST_VARS];  /* Array of ints to resume test */
    int enable_cache;               /* Flush the I/O server-side cache */
    int cache_size;
    int wait_time;                  /* How long to wait between tests? */
    int estimate_space;
    int gen_files; /* Do we precreate files before writing/reading? */
    int keep_files; /* Keep files after they are read? */
    int same_file; /* Use the same file for a group of repetitions? */
    int debug_mode;
    int tests_passed;
    int tests_failed;
    int atomicity; /* Set atomic mode or not */
    uint64_t debug_mask;
};

void print_usage(char *exefile);
int parse_args(int argc, char *argv[], struct test_params_s *test_params_p);
void print_settings(struct test_params_s *test_params_p);

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
