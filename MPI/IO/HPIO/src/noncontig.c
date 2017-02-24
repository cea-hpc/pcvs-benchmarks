/* (C) Northwestern University
 * See COPYING in the top-level directory . */
#include <getopt.h>
#include "mpi.h"
#include <mpi.h>
#include "test_params.h"
#include "test.h"
#include "cache_flush.h"
#include "space_requirement.h"
#include "files.h"
#include "output.h"


const int params_count[MAX_TEST] = {MAX_COUNT_COUNT, MAX_SIZE_COUNT, 
			      MAX_SPACING_COUNT, MAX_BAND_SINGLE_COUNT,
			      MAX_HUMAN_COUNT, MAX_DEFINED_COUNT,
			      MAX_BAND_SINGLE_COUNT};
#ifndef HPIO_SMALL
#define DEF_COUNT_IDX   1
#define DEF_SIZE_IDX    0
#define DEF_SPACING_IDX 4
const int region_count_arr[MAX_COUNT_COUNT] = 
    {2*1024, 16*1024};
const int region_size_arr[MAX_SIZE_COUNT] = 
    {8, 16, 32, 64, 128};
const int region_spacing_arr[MAX_SPACING_COUNT] = 
    {8, 16, 32, 64, 128};
#else
#define DEF_COUNT_IDX   2
#define DEF_SIZE_IDX    0
#define DEF_SPACING_IDX 3
const int region_count_arr[MAX_COUNT_COUNT] = {1, 2, 4, 8, 16};
const int region_size_arr[MAX_SIZE_COUNT] = {8, 16, 32, 64, 128};
const int region_spacing_arr[MAX_SPACING_COUNT] = {1, 2, 4, 8, 16};
#endif

const int band_single_arr[MAX_BAND_SINGLE_COUNT] = {-1};
const int check_single_arr[MAX_CHECK_SINGLE_COUNT] = {-1};

int human_arr[MAX_HUMAN_COUNT];
int defined_arr[MAX_DEFINED_COUNT];

/* Names of variables for the test. */

const int *params_arr_p[MAX_TEST] = {region_count_arr,
			       region_size_arr,
			       region_spacing_arr,
			       band_single_arr,
			       NULL,
			       NULL,
                               check_single_arr};

char *rw_name[MAX_RW] = {"write",
			 "read"};

char *test_type_name[MAX_TEST] = {"region_count",
				  "region_size",
				  "region_spacing",
				  "bandwidth_single",
				  "human",
				  "defined",
				  "check_single"};

char *test_param_name[MAX_TEST] = {"region_count",
				   "region_size",
				   "region_spacing",
				   "ct,sz,spacing",
				   "human",
				   "defined",
                                   "check_single"};

char *noncontig_type_name[MAX_NONCONTIG] = {"c-c",
					    "nc-c",
					    "c-nc",
					    "nc-nc"};

char *io_method_name[MAX_IO_METHOD] = {"individual",
				       "collective"};

char *pattern_dtype_name[MAX_PATTERN_DTYPE] = {"vector",
					       "struct"};

char *fsync_method_name[MAX_FSYNC_METHOD] = {"individual",
					     "collective"};

char *gen_files_name[MAX_GEN_FILES] = {"full",
				       "partial",
				       "none"};

char *gen_files_print[MAX_GEN_FILES] = {"full (always)",
					"partial (before reads)",
					"none (no file generation)"};

char *average_name[MAX_AVE] = {"average all",
			       "average all (without max or min)",
			       "best repetition"};

char *verify_name[MAX_VERIFY] = {"none",
				 "verify data",
				 "verify file"};

int main(int argc, char **argv)
{
    char output_info[MAX_FILENAME_SIZE];
    char output_header[MAX_FILENAME_SIZE];
    char output_min_results[MAX_FILENAME_SIZE];
    char output_results[MAX_FILENAME_SIZE];
    char output_failed[MAX_FILENAME_SIZE];

    int ret = -1, i, j, k, l, m, myid, numprocs;

    int hint_key_len, hint_val_len, hint_nkeys, flag = -1;
    char hint_key[MPI_MAX_INFO_KEY], *hint_val = NULL;
    struct test_params_s test_params;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    /* Parse arguments and pass them around */
    if (!myid)
	ret = parse_args(argc, argv, &test_params);
 

    params_arr_p[4] = human_arr;
    params_arr_p[5] = defined_arr;
    
    /* Setup human/defined test param arrays */
    for (i = 0; i < MAX_HUMAN_COUNT; i++)
	human_arr[i] = i;
    for (i = 0; i < MAX_DEFINED_COUNT; i++)
	defined_arr[i] = i;

    /* Check for repeating values in the test parameter arrays */
    for (i = 0; i < MAX_TEST; i++)
    {
	if (i != BAND_SINGLE && i != CHECK_SINGLE)
	{
	    for (j = 0; j < params_count[i]; j++)
	    {
		for (k = 0; k < params_count[i]; k++)
		{
		    if (((params_arr_p[i])[j] == (params_arr_p[i])[k]) &&
			j != k)
		    {
			debug_stderr("Repeated parameter "
				     "at %s[%d] and %s[%d].  Exiting.\n", 
				     test_type_name[i], j, 
				     test_type_name[i], k);
			ret = -1;
		    }
		}
	    }
	}
    }
    
    MPI_Bcast(&ret, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (ret != 0)
    {
	MPI_Finalize();
	return 0;
    }
    
    /* Pass/Set the arguments to all processes */
    MPI_Bcast(&test_params, sizeof(struct test_params_s), 
	      MPI_BYTE, 0, MPI_COMM_WORLD);
    test_params.myid = myid;
    if (myid)
    {
	test_params.info_p = malloc(sizeof(MPI_Info));
	if (!test_params.info_p)
	{
	    debug_stderr("malloc of info_p with size %d failed.\n",
			 sizeof(MPI_Info));
	    ret = -1;
	}
	MPI_Info_create(test_params.info_p);
    }

    /* Share MPI-IO hints */
    if (!myid)
	MPI_Info_get_nkeys(*(test_params.info_p), &hint_nkeys);

    MPI_Bcast(&hint_nkeys, 1, MPI_INT, 0, MPI_COMM_WORLD);
    for (i = 0; i < hint_nkeys; i++) 
    {
	if (!myid)
	{
	    MPI_Info_get_nthkey(*(test_params.info_p), i, hint_key);
	    hint_key_len = strlen(hint_key);
	    MPI_Info_get_valuelen(*(test_params.info_p), hint_key, 
				  &hint_val_len, &flag);
	    assert(flag);
	    
	    hint_val = malloc((hint_val_len + 1)*sizeof(char));
	    if (!hint_val)
	    {
		debug_stderr("hint_val malloc of size %d failed.\n",
			     hint_val_len);
		return -1;
	    }
	    
	    MPI_Info_get(*(test_params.info_p), hint_key, 
			 hint_val_len + 1, hint_val, &flag);
	    assert(flag);
	}
	MPI_Bcast(&hint_key_len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(hint_key, hint_key_len + 1, MPI_CHAR, 0, MPI_COMM_WORLD);
	MPI_Bcast(&hint_val_len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	if (myid)
	{
	    hint_val = malloc((hint_val_len + 1)*sizeof(char));
	    if (!hint_val)
	    {
		debug_stderr("hint_val malloc of size %d failed.\n",
			     hint_val_len);
		return -1;
	    }
	}
	MPI_Bcast(hint_val, hint_val_len + 1, MPI_CHAR, 0, MPI_COMM_WORLD);
	if (myid)
	    MPI_Info_set(*(test_params.info_p), hint_key, hint_val);

	free(hint_val);
    }
    if (test_params.io_method[COLLECTIVE] == TRUE)
    {
	MPI_Info_set(*(test_params.info_p), "romio_cb_write", "enable");
	MPI_Info_set(*(test_params.info_p), "romio_cb_read", "enable");
    }
    
    if (myid == 0)
	print_header(stdout, &test_params);

    if (test_params.estimate_space == TRUE)
    {
	if (myid == 0)
	    estimate_space(&test_params);

	MPI_Finalize();
	return 0;
    }

    if (test_params.dir_len != DEFAULT_PARAM_VAL)
    {
	if (myid != 0)
	{
	    if ((test_params.dir = 
		 (char *) malloc(test_params.dir_len + 1)) == NULL)
	    {
		debug_stderr("malloc test_params_dir of size %d failed\n",
			     test_params.dir_len + 1);
		return -1;
	    }
	}
	MPI_Bcast(test_params.dir, test_params.dir_len + 1, MPI_CHAR, 
		  0, MPI_COMM_WORLD);
    }

    if (test_params.output_dir_len != DEFAULT_PARAM_VAL)
    {
	if (myid != 0)
	{
	    if ((test_params.output_dir = 
		 (char *) malloc(test_params.output_dir_len + 1)) == NULL)
	    {
		debug_stderr("malloc test_params_output_dir of size %d "
			     "failed\n", test_params.output_dir_len + 1);
		return -1;
	    }
	}
	MPI_Bcast(test_params.output_dir, test_params.output_dir_len + 1, 
		  MPI_CHAR, 0, MPI_COMM_WORLD);
    }

    if (test_params.def_check_test_len != DEFAULT_PARAM_VAL)
    {
	if (myid != 0)
	{
	    if ((test_params.def_check_test = 
		 (char *) malloc(test_params.def_check_test_len + 1)) == NULL)
	    {
		debug_stderr("malloc test_params_def_check_test of size %d "
			     "failed\n", test_params.def_check_test_len + 1);
		return -1;
	    }
	}
	MPI_Bcast(test_params.def_check_test, test_params.def_check_test_len +
		  1, MPI_CHAR, 0, MPI_COMM_WORLD);
    }

    /* Generate filename and open up files for writing out
     * comprehensive performance results */
    create_output_filenames(output_info,
			    output_header, 
			    output_min_results,
			    output_results,
			    output_failed,
			    MAX_FILENAME_SIZE,
			    &test_params);
    test_params.output_info = fopen(output_info, "a");
    if (test_params.output_info == NULL)
    {
	debug_stderr("fopen %s failed\n", output_info);
	return -1;
    }
    if (myid == 0)
    {
	test_params.output_header = fopen(output_header, "w");
	if (test_params.output_header == NULL)
	{
	    debug_stderr("fopen %s failed\n", output_header);
	    return -1;
	}
	test_params.output_min_results = fopen(output_min_results, "w");
	if (test_params.output_min_results == NULL)
	{
	    debug_stderr("fopen %s failed\n", output_min_results);
	    return -1;
	}
	test_params.output_results = fopen(output_results, "w");
	if (test_params.output_results == NULL)
	{
	    debug_stderr("fopen %s failed\n", output_results);
	    return -1;
	}
	test_params.output_failed = fopen(output_failed, "w");
	if (test_params.output_failed == NULL)
	{
	    debug_stderr("fopen %s failed\n", output_failed);
	    return -1;
	}
    }

    if (myid == 0)
    {
	print_full_timing_header(test_params.output_header);
	print_header(test_params.output_info, &test_params);
    }

    /* Set own debug arguments */
    debug_mask = test_params.debug_mask;
    debug_mode = test_params.debug_mode;

    /* Generate the appropriate files prior to beginning the test */
    generate_files(&test_params);

    /* Run tests */
    i = test_params.resume_arr[VAR_RW];
    for (; i < MAX_RW; i++)
    {
	int param_val = -1;
	if (test_params.rw[i] == TRUE) /* Do the writes/reads */
	{
	    if (i == test_params.resume_arr[VAR_RW])
		j = test_params.resume_arr[VAR_TEST];
	    else 
		j = 0;
	    for (; j < MAX_TEST; j++)
	    {
		if (test_params.test[j] == TRUE)
		{
		    if (j == test_params.resume_arr[VAR_TEST])
			k = test_params.resume_arr[VAR_NONCONTIG];
		    else
			k = 0;
		    for (; k < MAX_NONCONTIG; k++)
		    {
			if (test_params.noncontig[k] == TRUE)
			{
			    if (k == test_params.resume_arr[VAR_NONCONTIG])
				l = test_params.resume_arr[VAR_IO_METHOD];
			    else
				l = 0;
			    for (; l < MAX_IO_METHOD; l++)
			    {
				if (test_params.io_method[l] == TRUE)
				{
				    if (test_params.myid == 0)
				    {
					preprint_min_output(
					    stdout, i, j, k, l);
					preprint_min_output(
					    test_params.output_min_results, 
					    i, j, k, l);
				    }
				    if (l == test_params.resume_arr[
					    VAR_IO_METHOD])
					m = test_params.resume_arr[
					    VAR_PARAM_IDX];
				    else
					m = 0;
				    for (; m < params_count[j]; m++)
				    {
					if (j == CHECK_SINGLE)
					    param_val = 
						test_params.
						def_check_test_param_val;
					else
					    param_val = (params_arr_p[j])[m];
					ret = run_test(i, j, k, l, param_val,
						       TEST_MODE,
						       &test_params);
					if (ret != 0)
					    return -1;
				    }
				    if (test_params.myid == 0)
				    {
					debug_stdout_stream(
					    test_params.output_min_results, 
					    "\n");
				    }
				}
			    }
			}
		    }
		}
	    }
	}      
	/* Flush the server cache */
	if (test_params.rw[READ] == TRUE &&
	    test_params.enable_cache == TRUE &&
	    i == WRITE)
	{
	    char tmp_filename[MAX_FILENAME_SIZE];
		
	    if (test_params.dir == NULL)
		sprintf(tmp_filename, "%s",
			"cache");
	    else
		sprintf(tmp_filename, "%s/%s",
			test_params.dir,
			"cache");
	    
	    cache_flush_ind_all(test_params.myid,
				test_params.numprocs,
				test_params.cache_size,
				tmp_filename);
	}
    }

    if (test_params.rw[READ] == TRUE &&
	test_params.enable_cache == TRUE)
    {
	char tmp_filename[MAX_FILENAME_SIZE];
	
	if (test_params.dir == NULL)
	    sprintf(tmp_filename, "%s",
		    "cache");
	else
	    sprintf(tmp_filename, "%s/%s",
		    test_params.dir,
		    "cache");
	
	cache_flush_ind_all_remove_files(test_params.myid,
					 test_params.numprocs,
					 tmp_filename);
    }

    if (test_params.verify > VERIFY_NONE)
    {
	int total_tests = 
	    test_params.tests_passed + test_params.tests_failed;

	MPI_Barrier(MPI_COMM_WORLD);
	if (myid == 0)
	{
	    if (test_params.tests_failed == 0)
	    {
		debug_stdout_stream(
		    test_params.output_info,
		    "**All (%d) test(s) passed without any errors!**\n", 
		    total_tests);
	    }
	    else
	    {
		debug_stdout_stream(
		    test_params.output_info,
		    "**Only %d of %d test(s) passed (%d tests failed)!**\n",
		    test_params.tests_passed, total_tests, 
		    test_params.tests_failed);
	    }
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /* Close the output files */
    if (fclose(test_params.output_info) != 0)
    {
	debug_stderr("Error closing info file\n");
	return -1;
    }
    if (myid == 0)
    {
	if (fclose(test_params.output_header) != 0)
	{
	    debug_stderr("Error closing header file\n");
	    return -1;
	}
 	if (fclose(test_params.output_min_results) != 0)
	{
	    debug_stderr("Error closing min results file\n");
	    return -1;
	}
 	if (fclose(test_params.output_results) != 0)
	{
	    debug_stderr("Error closing results file\n");
	    return -1;
	}
 	if (fclose(test_params.output_failed) != 0)
	{
	    debug_stderr("Error closing failed file\n");
	    return -1;
	}
    }

    MPI_Info_free(test_params.info_p);
    free(test_params.info_p);
	
    free(test_params.dir);
    free(test_params.output_dir);
    free(test_params.def_check_test);
    MPI_Finalize();
    return 0;
}

void print_usage(char *exefile)
{
    debug_stdout(
"Usage: %s [OPTION]... [VAR=VALUE]...\n\n"
"  -h  --help            Display this help and exit.\n"
"  -d  --dir             Use this directory to create tmp files.\n"
"  -o  --output_dir      Use this directory to create output files\n"
"  -c  --region_count    Default region count (default %d).\n"
"  -s  --region_size     Default region size (default %d).\n"
"  -p  --region_spacing  Default region spacing - (default %d).\n"
"                        Actual separation between aggregate file regions\n"
"                        (i.e. procs=2, region_count=2, region_size=1,\n"
"                        region_spacing=3)\n"
"                        0___1___0___1   <--- should end up like this.\n"
"  -r  --reps            Choose the number of repetitions (default 1).\n"
"  -a  --average_method  0 - (default) Average all runs.\n"
"                        1 - Average all runs (except high and low).\n"
"                        2 - Choose the best repetition (based on aggregate\n"
"                        I/O time)\n"	    
"  -b  --band_tests      Use array of bandwidth tests mode (cannot be used\n"
"                        with single bandwidth test mode (-B)).  Pick an\n"
"                        array of bandwidth tests to be run.  For example\n"
"                        to run region count and region size tests use\n"
"                        -b 110.\n"
"                        100 - region count\n"
"                        010 - region size\n"
"                        001 - region spacing\n"
"  -B  --band_single     Run in single bandwidth test mode which uses\n"
"                        default test parameters which can be changed with\n"
"                        options (-c -s -p).  Single bandwidth test mode\n"
"                        cannot be used with the array of bandwidth tests\n"
"                        mode (-b).\n"
"  -x  --check_tests     Use array of check tests mode (cannot be used\n"
"                        with single check test mode (-X)).  Pick an\n"
"                        array of check tests to be run.  For example\n"
"                        to run human mode use -x 10.\n"
"                        10 - human mode\n"
"                        01 - defined mode\n"
"  -X  --check_single    Run in single check test mode which uses\n"
"                        default test parameters which can be changed with\n"
"                        options (-c -s -p).  Single bandwidth test mode\n"
"                        cannot be used with the array of bandwidth tests\n"
"                        mode (-b).  Unlike -B, it requires an argument\n"
"                        which describes the name of the check test.\n"
"                        For example, -X struct_lb_ub.\n"
"  -n  --noncontig       Choose an array of the noncontiguous types for\n"
"                        testing.  For example, to test noncontiguous\n"
"                        memory to contiguous file and contiguous memory to\n"
"                        noncontiguous file, use -n 0110.\n"
"                               Mem           | File          \n"
"                        1000 - contiguous    | contiguous    \n"
"                        0100 - noncontiguous | contiguous    \n"
"                        0010 - contiguous    | noncontiguous \n"
"                        0001 - noncontiguous | noncontiguous \n"
"  -m  --io_methods      Choose an array of I/O methods for testing.  For\n"
"                        example, to test invidiual I/O and not collective\n"
"                        I/O use -m 10 (default).\n"
"                        10 - individual\n"
"                        01 - collective\n"
"  -D  --pattern_dtype   Choose which datatypes to use for creating the\n"
"                        access pattern noncontiguity when memory and/or\n"
"                        file is noncontiguous.\n"
"                        0 - (default) uses MPI_VECTOR with count = 1\n"
"                        1 - uses MPI_STRUCT of MPI_CHAR and MPI_UB with\n"
"                        count = region_count\n"
"  -O  --readwrite       Choose whether to write and/or read.  Also see\n"
"                        --generate_files (-g) option for help when doing.\n"
"                        read-only testing.\n" 
"                        10 - write\n"
"                        01 - read\n"
"  -f  --fsync           Choose whether to fsync (force data to storage)\n"
"                        after write tests.\n"
"                        0 - no fsync\n"
"                        1 - (default) fsync on\n"
"  -F  --fsync_method    If fsync is on, an fsync method can be chosen.\n"
"                        0 - (default) Testing will causes processes to\n"
"                        individually open the file and each call fsync.\n"
"                        When testing two phase I/O, processes will\n"
"                        collectively open the file, which allows an\n"
"                        optimization to reduce the total number of fsync\n"
"                        calls to 1.\n"
"                        1 - Using any I/O method will cause processes to\n"
"                        open the file collectively which allows the use of\n"
"                        the collective fsync optimization.\n"
"  -w  --wait_time       Input the number of seconds to wait in between\n"
"                        repetitions (default is 1 second).\n"
"  -g  --generate_files  Choose whether to generate full, partial or no\n"
"                        files before any testing begins.\n"
"                        0 - Enable the full generation of files before\n"
"                        any read or write operation.  This reduces the\n"
"                        effects of file creation and read-modify-writes.\n"
"                        It also provides files for reading when doing\n"
"                        read-only testing.\n"
"                        1 - (default) Ensures the partial generation of\n"
"                        files before any read operation.  The\n"
"                        major differences between option 0 and 1 are that\n"
"                        option 1 only writes necessary data to the file\n"
"                        for reading instead of creating the full file and\n"
"                        option 1 only creates files prior to reading.\n"
"                        2 - Disable generation of any files before any\n"
"                        read or write operation.  It may cause\n"
"                        read-only testing to fail if the files to be\n"
"                        read do not exist (i.e. have not been created\n"
"                        previously with the write mode\n"
"  -k  --keep_files      Keep all created files.  No file deletion prior\n"
"                        to writing and no file deletion after reading.\n"
"  -S  --same_file       0 - (default) Use seperate files for each \n"
"                        repetition.\n"
"                        1 - Use the same file for a set of\n"
"                        repetitions (better for PFRs and buffered\n"
"                        reads testing).\n"
"  -e  --enable_flush    Enable flushing the system buffer cache by simply\n"
"                        writing out enough data to fill the aggregate\n"
"                        cache of the storage system.  Default is off.\n"
"  -C  --cache_size      If --enable_flush (-e) option is enabled, input\n"
"                        a cache size in MBytes large enough to clear the\n"
"                        cache of the storage system (default 2 GBytes).\n"
"  -v  --verify          0 - No verification (default)\n"
"                        1 - Verifcation of accessed data\n"
"                        2 - Verification of all data in file\n"	    
"  -R  --resume          Use with array of tests mode to restart a suite of\n"
"                        tests from a failed point.  Every parameter\n"
"                        is specified in the form:\n"
"                        o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int].\n"
"                        For example, if all read/write modes, tests\n"
"                        noncontiguous types, and noncontiguous\n"
"                        methods were selected, and the test\n"
"                        failed after writing in the region size\n"
"                        test at 4096 with nc-c with two phase I/O,\n"
"                        use -R o10t010n0100m001000v4096.\n"  
"  -E  --estimate_space  Do no testing, only estimate the maximum space\n"
"                        required to comoplete the testing\n"
"  -A  --atomicity       Set MPI file atomicity to 0 (default) or 1\n"
"  -H  --mpi-io-hint     Set as many MPI-IO hints as is desired through \n"
"                        repeated use.  The interface is key=value.  For \n"
"                        example, to turn off data sieving for writes in \n"
"                        ROMIO, use \"-H romio_ds_write=enable\"\n", 
exefile,
region_count_arr[DEF_COUNT_IDX],
	region_size_arr[DEF_SIZE_IDX],
	region_spacing_arr[DEF_SPACING_IDX]);
}

static int convert_var_index(int var)
{
    int mask = 1, idx = -1, i;
    
    /* Begin at the highest mask */
    for (i = 0; i < 9; i++)
	mask *= 10;

    for (i = 0; i < 10; i++)
    {
	if (var / mask == 1)
	{
	    if (idx == -1)
	    {
		idx = 9 - i;
		var -= mask;
	    }
	    else
	    {
		debug_stderr("convert_var_index: multiple indexes found "
			     "at %d and %d\n", idx, 9 - i);
		return -1;
	    }
	}
	mask /= 10;
    }
    return idx;
}

static int set_resume_index(char *str_p, int parse_char, int max_var_len,
			    int *resume_var_p)
{
    int tmp_test = -1, idx = -1, i;
    char *str_begin_p = NULL, *substr_p = NULL;

    str_begin_p = strchr(str_p, parse_char);
    if (str_begin_p == NULL)
    {
	debug_stderr("set_resume_index: Resume parameter (-R) requires "
		     "form o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int].  It is "
		     "missing the \'%c\' arg.\n", parse_char);
	return -1;
    }
    substr_p = calloc(max_var_len, sizeof(char));
    strncpy(substr_p, str_begin_p + 1, max_var_len);
    for (i = 0; i < max_var_len; i++)
    {
	if (substr_p[i] != '0' && substr_p[i] != '1')
	{
	    debug_stderr("set_resume_index: Resume parameter (-R) requires "
			 "form o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int].  The "
			 "\'%c\' arg requires a proper length of %d and "
			 "idx %d of %c is invalid.\n", parse_char, max_var_len,
			 i, (char) substr_p[i]);
	    return -1;
	}
    }
    tmp_test = atoi(substr_p);
    idx = convert_var_index(tmp_test);
    if (idx < 0)
    {
	debug_stderr("set_resume_index: Resume parameter (-R) requires "
		     "form o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int]. \'%c\' arg "
		     "needs one set position.\n", parse_char);
	return -1;
    }
    *resume_var_p = max_var_len - idx - 1;
    mask_stdout(MASK_RESUME, "set_resume_index: Param \'%c\' with string %s "
		"set to index %d\n", (char) parse_char, substr_p, 
		*resume_var_p);
    free(substr_p);
    return 0;
}

static int set_resume_var(char *str_p, int parse_char, int max_var_len,
			  int *param_arr, int param_arr_ct,
			  int *resume_var_p)
{
    int tmp_test = -1, i;
    char *str_begin_p = NULL, *substr_p = NULL;

    str_begin_p = strchr(str_p, parse_char);
    if (str_begin_p == NULL)
    {
	debug_stderr("set_resume_var: Resume parameter (-R) requires "
		     "form o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int].  It is "
		     "missing the \'%c\' arg.\n", parse_char);
	return -1;
    }
    substr_p = calloc(max_var_len, sizeof(char));
    strncpy(substr_p, str_begin_p + 1, max_var_len);
    
    tmp_test = atoi(substr_p);

    /* Look in the correct test array for a matching integer */
    *resume_var_p = -1;
    for (i = 0; i < param_arr_ct; i++)
    {
	if (param_arr[i] == tmp_test)
	    *resume_var_p = i;
    }

    if (*resume_var_p < 0)
    {
	debug_stderr("set_resume_var: Resume parameter (-R) requires "
		     "form o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int]. \'%c\' arg "
		     "with int %d did not match any members of the test "
		     "array.\n", parse_char, tmp_test);
	return -1;
    }

    mask_stdout(MASK_RESUME, "set_resume_var:  Param \'%c\' with string %s "
		"set to index %d\n", (char) parse_char, substr_p, 
		*resume_var_p);

    free(substr_p);
    return 0;
}

static int decode_args(int arg, int *arg_arr, int arg_arr_ct)
{
    int i, tmp_arg = arg;
    int mask = 1;

    if (arg_arr_ct >= 10)
    {
	debug_stderr("decode_arg: arg_arr_ct is too big for an int.\n");
	return -1;
    }

    /* Begin at the highest mask */
    for (i = 1; i < arg_arr_ct; i++)
	mask *= 10;
    
    for (i = 0; i < arg_arr_ct; i++)
    {
	mask_stdout(MASK_PARSE_ARGS, 
		    "decode_args: tmp_arg = %d mask = %d\n", tmp_arg, mask);
	
	if (tmp_arg / mask == 1)
	{
	    arg_arr[i] = TRUE;
	    tmp_arg -= mask;
	}
	else
	    arg_arr[i] = FALSE;

	mask /= 10;
    }
    return -1;
}

#define MAX_ARG INT_MAX
int parse_args(int argc, char *argv[], struct test_params_s *test_params_p) 
{
    char *debug_env = NULL;
    int i, j, k, l, tmp_test = 0, test_arr_used = 0, test_single_used = 0;
    int def_count_idx, def_size_idx, def_spacing_idx;
    int hint_key_len, hint_val_len;
    char *break_p, hint_key[MPI_MAX_INFO_KEY], *hint_val;

    static struct option longopts[] =
	{
	    {"help", 0, NULL, 'h'},
	    {"dir", 1, NULL, 'd'}, 
	    {"output_dir", 1, NULL, 'o'},
	    {"region_count", 1, NULL, 'c'}, 
	    {"region_size", 1, NULL, 's'}, 
	    {"region_spacing", 1, NULL, 'p'}, 
	    {"reps", 1, NULL, 'r'}, 
	    {"average_method", 1, NULL, 'a'}, 
	    {"band_tests", 1, NULL, 'b'},
	    {"band_single", 0, NULL, 'B'},
	    {"check_tests", 1, NULL, 'x'},
	    {"check_single", 1, NULL, 'X'},
	    {"noncontig", 1, NULL, 'n'},
	    {"io_methods", 1, NULL, 'm'},
	    {"pattern_dtype", 1, NULL, 'D'},
	    {"readwrite", 1, NULL, 'O'},
	    {"fsync", 1, NULL, 'f'},
	    {"fsync_method", 1, NULL, 'F'},
	    {"wait_time", 1, NULL, 'w'},
	    {"generate_files", 1, NULL, 'g'},
	    {"keep_files", 0, NULL, 'k'},
	    {"same_file", 1, NULL, 'S'},
	    {"enable_flush", 0, NULL, 'e'},
	    {"cache_size", 1, NULL, 'C'},	    
	    {"verify", 1, NULL, 'v'},
	    {"resume", 1, NULL, 'R'},
	    {"estimate_space", 0, NULL, 'E'},
	    {"atomicity", 1, NULL, 'A'},
	    {"mpi-io-hint", 1, NULL, 'H'},
	    {0,0,0,0}
	};
    
    /* Index of current long option into opt_lng array */
    int option_index = 0; 
    int err = -1;
    char *optstring =
	":hd:o:c:s:p:r:a:b:Bx:X:n:m:D:O:f:F:w:g:kS:eC:v:R:EA:H:";

    memset(test_params_p, DEFAULT_PARAM_VAL, sizeof(struct test_params_s));
    MPI_Comm_size(MPI_COMM_WORLD, &(test_params_p->numprocs));

    test_params_p->dir = NULL;
    test_params_p->output_dir = NULL;
    test_params_p->def_check_test = NULL;
    test_params_p->rep_max_time = -1;
    test_params_p->tests_passed = 0;
    test_params_p->tests_failed = 0;
    test_params_p->atomicity = 0;

    test_params_p->info_p = malloc(sizeof(MPI_Info));
    if (!test_params_p->info_p)
    {
	debug_stderr("malloc of info_p with size %d failed.\n",
		     sizeof(MPI_Info));
	return -1;
    }
    MPI_Info_create(test_params_p->info_p);

    while (1)
    {
	err = getopt_long(argc, argv, optstring, longopts, &option_index);
	if (err == -1)
	    break;
	switch (err)
	{
	    case 'h':
		print_usage(argv[0]);
		return -1;
	    case 'd':
		test_params_p->dir_len = strlen(optarg);
		if ((test_params_p->dir = malloc(
			 test_params_p->dir_len + 1)) == NULL)
		{
		    debug_stderr("malloc dir of size %Ld failed\n",
				 test_params_p->dir_len + 1);
		    return -1;
		}
		strcpy(test_params_p->dir, optarg);
		break;
	    case 'o':
		test_params_p->output_dir_len = strlen(optarg);
		if ((test_params_p->output_dir = malloc(
			 test_params_p->output_dir_len + 1)) == NULL)
		{
		    debug_stderr("malloc dir of size %Ld failed\n",
				 test_params_p->output_dir_len + 1);
		    return -1;
		}
		strcpy(test_params_p->output_dir, optarg);
		break;
	    case 'c':
		test_params_p->def_region_params[REGION_COUNT] = 
		    atoi(optarg);
		if (test_params_p->def_region_params[REGION_COUNT] < 1 ||
		    test_params_p->def_region_params[REGION_COUNT] > MAX_ARG)
		{
		    debug_stderr(
			"%d region_count invalid\n",
			test_params_p->def_region_params[REGION_COUNT]);
		    return -1;
		}
		break;
	    case 's':
		test_params_p->def_region_params[REGION_SIZE] = atoi(optarg);
		if (test_params_p->def_region_params[REGION_SIZE] < 1 ||
		    test_params_p->def_region_params[REGION_SIZE] > MAX_ARG)
		{
		    debug_stderr(
			"%d region_size invalid\n",
			test_params_p->def_region_params[REGION_SIZE]);
		    return -1;
		}
		break;
	    case 'p':
		test_params_p->def_region_params[REGION_SPACING] = atoi(optarg);
		if (test_params_p->def_region_params[REGION_SPACING] < 0 ||
		    test_params_p->def_region_params[REGION_SPACING] > MAX_ARG)
		{
		    debug_stderr(
			"%d region_spacing invalid\n",
			test_params_p->def_region_params[REGION_SPACING]);
		    return -1;
		}
		break;
	    case 'r':
		test_params_p->reps = atoi(optarg);
		if (test_params_p->reps < 1 ||
		    test_params_p->reps > MAX_ARG)
		{
		    debug_stderr("%d reps invalid\n",
				 test_params_p->reps);
		    return -1;
		}
		break;
	    case 'a':
		test_params_p->average_method = atoi(optarg);
		if (test_params_p->average_method < 0 ||
		    test_params_p->average_method > MAX_AVE)
		{
		    debug_stderr("%d average_method invalid\n",
				 test_params_p->average_method);
		    return -1;
		}
		break;
	    case 'b':
		tmp_test = atoi(optarg);
		if (tmp_test > 111 || tmp_test <= 0)
		{
		    debug_stderr("%d bandwidth test array invalid\n",
				 tmp_test);
		    return -1;
		}
		/* Skip the correctness tests and single bandwidth test */ 
		tmp_test *= 10000; 
		decode_args(tmp_test, test_params_p->test, 
			    MAX_TEST);
		break;
	    case 'B':
		test_params_p->test[BAND_SINGLE] = TRUE;
		break;
	    case 'x':
		tmp_test = atoi(optarg);
		if (tmp_test > 11 || tmp_test <= 0)
		{
		    debug_stderr("%d check test array invalid\n",
				 tmp_test);
		    return -1;
		}
		/* Skip the correctness tests and single bandwidth test */ 
		tmp_test *= 10; 
		decode_args(tmp_test, test_params_p->test, 
			    MAX_TEST);
		break;
	    case 'X':
		test_params_p->def_check_test_len = strlen(optarg);
		if ((test_params_p->def_check_test = malloc(
			 test_params_p->def_check_test_len + 1)) == NULL)
		{
		    debug_stderr("malloc def_check_test of size %d failed\n",
				 test_params_p->def_check_test_len + 1);
		    return -1;
		}
		strcpy(test_params_p->def_check_test, optarg);
		test_params_p->test[CHECK_SINGLE] = TRUE;
		break;
	    case 'n':
		tmp_test = atoi(optarg);
		decode_args(tmp_test, test_params_p->noncontig,
			    MAX_NONCONTIG);
		break;
	    case 'm':
		tmp_test = atoi(optarg);
		decode_args(tmp_test, test_params_p->io_method, 
			    MAX_IO_METHOD);
		break;
	    case 'D':
		test_params_p->pattern_dtype = atoi(optarg);
		if (test_params_p->pattern_dtype != 0 &&
		    test_params_p->pattern_dtype != 1)
		{
		    debug_stderr("pattern dtype (%d) must be 0 or 1\n",
				 test_params_p->pattern_dtype);
		}
		break;
	    case 'O':
		tmp_test = atoi(optarg);
		decode_args(tmp_test, test_params_p->rw, MAX_RW);
		break;
	    case 'f':
                test_params_p->enable_fsync = atoi(optarg);
                if (test_params_p->enable_fsync != 0 &&
                    test_params_p->enable_fsync != 1)
                {
                    debug_stderr("enable fsync (%d) must be 0 or 1\n",
				 test_params_p->enable_fsync);
                    return -1;
                }
		break;
	    case 'F':
		test_params_p->fsync_method = atoi(optarg);
		if (test_params_p->fsync_method != INDIVIDUAL &&
		    test_params_p->fsync_method != COLLECTIVE)
		{
		    debug_stderr("fsync_method must be either 0 or 1\n");
		    return -1;
		}
		break;
	    case 'w':
		test_params_p->wait_time = atoi(optarg) * 1000000;
		if (test_params_p->wait_time < 0)
		{
		    debug_stderr("wait_time must be positive\n");
		    return -1;
		}
		break;
	    case 'g':
		test_params_p->gen_files = atoi(optarg);
		if (test_params_p->gen_files != GEN_FULL &&
		    test_params_p->gen_files != GEN_PARTIAL &&
		    test_params_p->gen_files != GEN_NONE)
		{
		    debug_stderr("generate files must be 0, 1, or 2\n");
		}
		break;
	    case 'k':
		test_params_p->keep_files = TRUE;
		break;
	    case 'S':
		test_params_p->same_file = atoi(optarg);
		if (test_params_p->same_file != 0 &&
		    test_params_p->same_file != 1)
		{
		    debug_stderr("Same file parameter %d must be 0 or 1\n",
				 test_params_p->same_file);
		    return -1;
		}
		break;
	    case 'e':
		test_params_p->enable_cache = TRUE;
		break;
	    case 'C':
		test_params_p->cache_size = atoi(optarg);
		if (test_params_p->cache_size < 1 ||
		    test_params_p->cache_size > MAX_ARG)
		{
		    debug_stderr("%d cache_size invalid\n",
				 test_params_p->cache_size);
		    return -1;
		}
		break;
	    case 'v':
		test_params_p->verify = atoi(optarg);
		if (test_params_p->verify < 0 ||
		    test_params_p->verify >= MAX_VERIFY)
		{
		    debug_stderr("%d verify mode invalid\n",
				 test_params_p->verify);
		    return -1;
		}
		break;
	    case 'R':
		/* String process the 'R' optarg to separate the
		 * arguments into their respective array values.  It
		 * appears in the form:
		 * o[xx]t[xxx]n[xxxx]m[xxxxxx]v[int] */
		test_params_p->enable_resume = TRUE;
		tmp_test = set_resume_index(
		    optarg, 'O', MAX_RW, 
		    &(test_params_p->resume_arr[VAR_RW]));
		if (tmp_test < 0)
		    return -1;
		tmp_test = set_resume_index(
		    optarg, 'b', MAX_TEST - 1, 
		    &(test_params_p->resume_arr[VAR_TEST]));
		if (tmp_test < 0)
		    return -1;
		tmp_test = set_resume_index(
		    optarg, 'n', MAX_NONCONTIG, 
		    &(test_params_p->resume_arr[VAR_NONCONTIG]));
		if (tmp_test < 0)
		    return -1;
		tmp_test = set_resume_index(
		    optarg, 'm', MAX_IO_METHOD, 
		    &(test_params_p->resume_arr[VAR_IO_METHOD]));
		if (tmp_test < 0)
		    return -1;
		tmp_test = set_resume_var(
		    optarg, 'v', HINT_STR_MAX, 
		    params_arr_p[test_params_p->resume_arr[VAR_TEST]],
		    params_count[test_params_p->resume_arr[VAR_TEST]],
		    &(test_params_p->resume_arr[VAR_PARAM_IDX]));
		if (tmp_test < 0)
		    return -1;
		break;
	    case 'E':
		test_params_p->estimate_space = TRUE;
		break;
	    case 'A':
		test_params_p->atomicity = atoi(optarg);
		if (test_params_p->atomicity != 0 &&
		    test_params_p->atomicity != 1)
		{
		    debug_stderr("Atomicity parameter (%d) must be 0 or 1\n",
				 test_params_p->atomicity);
		    return -1;
		}
		break;
	    case 'H':
		break_p = index(optarg, '=');
		if (!break_p)
		{
		    debug_stderr("Hint %s does not contain a '='.\n",
				 optarg);
		    return -1;
		}
		hint_key_len = break_p - optarg;
		hint_val_len = strlen(optarg) - hint_key_len - 1;
		hint_val = malloc((hint_val_len + 1)*sizeof(char));
		if (!hint_val)
		{
		    debug_stderr("hint_val malloc of size %d failed.\n",
				 hint_val_len);
		    return -1;
		}
		strncpy(hint_key, optarg, hint_key_len);
		strncpy(hint_val, break_p + 1, hint_val_len);
		hint_key[hint_key_len] = '\0';
		hint_val[hint_val_len] = '\0';
		MPI_Info_set(*(test_params_p->info_p), hint_key, hint_val);
		free(hint_val);
		break;
	    default:
		debug_stderr("Option -%c is invalid\n", 
			     (char) optopt);
		print_usage(argv[0]);
		return -1;
	}
    }

    /* Set parameters values if none have been filled in. */
    debug_stdout("Filling in parameters which have not been specified by "
		 "the user...\n");

    def_count_idx   = DEF_COUNT_IDX;
    def_size_idx    = DEF_SIZE_IDX;
    def_spacing_idx = DEF_SPACING_IDX;
    assert(def_count_idx   < MAX_COUNT_COUNT &&
	   def_size_idx    < MAX_SIZE_COUNT &&
	   def_spacing_idx < MAX_SPACING_COUNT);
    debug_stdout("** Initializing filename prefix to \'t\'\n");
    snprintf(test_params_p->filename_prefix, MAX_FILENAME_SIZE,
	     "%s", "t");
    /* Assign default values from the test array parameters without
     * overstepping array boundaries */
    if (test_params_p->def_region_params[REGION_COUNT] == DEFAULT_PARAM_VAL)
    {
	test_params_p->def_region_params[REGION_COUNT] = 
	    region_count_arr[def_count_idx];
	debug_stdout("** Initializing default region count = %d\n",
		     region_count_arr[def_count_idx]);
    }
    /* Worse case is scientific application writes one double at a time */
    if (test_params_p->def_region_params[REGION_SIZE] == DEFAULT_PARAM_VAL)
    {
	test_params_p->def_region_params[REGION_SIZE] = 
	    region_size_arr[def_size_idx];
	debug_stdout("** Initializing default region size = %d\n",
		     region_size_arr[def_size_idx]);
    }
    /* A good application will have many variables, which means a lot of 
     * spacing in between them.  In our default case, 16 doubles */
    if (test_params_p->def_region_params[REGION_SPACING] == DEFAULT_PARAM_VAL)
    {
	test_params_p->def_region_params[REGION_SPACING] =
	    region_spacing_arr[def_spacing_idx];
	debug_stdout("** Initializing default region spacing = %d\n",
		     region_spacing_arr[def_spacing_idx]);
    }
    if (test_params_p->reps == DEFAULT_PARAM_VAL)
    {
	test_params_p->reps = 1;
	debug_stdout("** Initializing default reps = %d\n",
		     test_params_p->reps);
    }
    if (((int) test_params_p->rep_max_time) == DEFAULT_PARAM_VAL)
    {
	/* 10 minutes */
	test_params_p->rep_max_time = 10 * 60.0;
	debug_stdout("** Initializing default rep max time = %f\n",
		     test_params_p->rep_max_time);
    }
    if (test_params_p->average_method  == DEFAULT_PARAM_VAL)
    {
	test_params_p->average_method = 0;
	debug_stdout("** Initializing default average method = %s\n",
		     average_name[test_params_p->average_method]);
    }
    if (test_params_p->rw[WRITE] == DEFAULT_PARAM_VAL &&
	test_params_p->rw[READ] == DEFAULT_PARAM_VAL &&
	test_params_p->estimate_space != TRUE)
    {
	/* At least do a write */
	test_params_p->rw[WRITE] = 1;
	test_params_p->rw[READ]  = 0;
	debug_stdout("** Initializing to do write testing (no read)\n");
    }
    if (test_params_p->wait_time == DEFAULT_PARAM_VAL)
    {
	test_params_p->wait_time = 1000000;
	debug_stdout("** Initializing to use 1 second wait time\n");
    }
    if (test_params_p->enable_fsync == DEFAULT_PARAM_VAL)
    {
	test_params_p->enable_fsync = 1;
	debug_stdout("** Initializing to use fsync\n");
    }
    if (test_params_p->fsync_method == DEFAULT_PARAM_VAL)
    {
	test_params_p->fsync_method = INDIVIDUAL;
	debug_stdout("** Initializing to use INDIVIDUAL fsync method\n");
    }
    if (test_params_p->pattern_dtype == DEFAULT_PARAM_VAL)
    {
	test_params_p->pattern_dtype = VECTOR;
	debug_stdout("** Initializing to use VECTOR pattern type\n");
    }
    if (test_params_p->gen_files == DEFAULT_PARAM_VAL)
    {
	test_params_p->gen_files = GEN_PARTIAL;
	debug_stdout("** Initializing use of partial generate file "
		     "option if necessary\n");
    }
    if (test_params_p->keep_files == DEFAULT_PARAM_VAL)
    {
	test_params_p->keep_files = FALSE;
	debug_stdout("** Initializing to not keep created files that "
		     "have been read\n");
    }
    if (test_params_p->same_file == DEFAULT_PARAM_VAL)
    {
	test_params_p->same_file = 0;
	debug_stdout("** Initializing to use different files per run\n");
    }
    if (test_params_p->cache_size == DEFAULT_PARAM_VAL)
    {
	/* 2 GB */
	test_params_p->cache_size = 2048;
	debug_stdout("** Initializing cache size of 2 GBytes\n");
    }
    if (test_params_p->verify == DEFAULT_PARAM_VAL)
    {
	/* No verification */
	test_params_p->verify = FALSE;
	debug_stdout("** Initializing verify mode off\n");
    }
    if (test_params_p->enable_cache == DEFAULT_PARAM_VAL)
    {
	test_params_p->enable_cache = FALSE;
	debug_stdout("** Initializing cache flush off\n");
    }
    if (test_params_p->enable_resume == DEFAULT_PARAM_VAL)
    {
	test_params_p->enable_resume = FALSE;
	debug_stdout("** Initializing enable resume off\n");
	for (i = 0; i < MAX_TEST_VARS; i++)
	    test_params_p->resume_arr[i] = 0;
    }
    if (test_params_p->estimate_space == DEFAULT_PARAM_VAL)
    {
	test_params_p->estimate_space = FALSE;
	debug_stdout("** Initializing to not estimate space\n");
    }

    /* Verify mode enables all nc methods, I/O methods, or tests that
     * are unspecified.  If single mode test is on, then test arrays
     * are not used, the converse is also true.  */

    if (test_params_p->verify > VERIFY_NONE)
    {
	int found = 0;
	test_params_p->wait_time = 0;
	debug_stdout("** Verify forces wait time to %d.\n",
		     test_params_p->wait_time);
	/* Check for test usage */
	found = TRUE;
	for (i = 0; i < MAX_TEST; i++)
	    if (i != BAND_SINGLE && i != CHECK_SINGLE)
		if (test_params_p->test[i] == DEFAULT_PARAM_VAL)
		    found = FALSE;

	if (found == FALSE && 
	    test_params_p->test[BAND_SINGLE] != TRUE &&
	    test_params_p->test[CHECK_SINGLE] != TRUE)
	{
	    debug_stdout("** Verify forces arrays of all bandwidth and "
			 "check tests on (no bandwidth or check single test "
			 "modes) if no tests were selected.\n");
	    for (i = 0; i < MAX_TEST; i++)
	    {
		if (i != BAND_SINGLE && i != CHECK_SINGLE)
		    test_params_p->test[i] = TRUE;
		else
		    test_params_p->test[i] = FALSE;
	    }
	}
	/* Check for read/write usage */
	found = TRUE;
	for (i = 0; i < MAX_RW; i++)
	    if (test_params_p->rw[i] == DEFAULT_PARAM_VAL)
		found = FALSE;
	
	if (found == FALSE)
	{
	    debug_stdout("** Verify forces both write/read on "
			 "if not expicitly stated\n");
	    for (i = 0; i < MAX_RW; i++)
		test_params_p->rw[i] = TRUE;
	}
	/* Check for noncontig type usage */
	found = TRUE;
	for (i = 0; i < MAX_NONCONTIG; i++)
	    if (test_params_p->noncontig[i] == DEFAULT_PARAM_VAL)
		found = FALSE;

	if (found == FALSE)
	{
	    debug_stdout("** Verify forces all noncontiguous methods on "
			 "if not expicitly stated\n");
	    for (i = 0; i < MAX_NONCONTIG; i++)
		test_params_p->noncontig[i] = TRUE;
   	}
	/* Check for I/O method usage */
	found = TRUE;
	for (i = 0; i < MAX_IO_METHOD; i++)
	    if (test_params_p->io_method[i] == DEFAULT_PARAM_VAL)
		found = FALSE;
	
	if (found == FALSE)
	{
	    debug_stdout("** Verify forces all I/O methods on "
			 "(excluding experimental two phase PFRs) if not "
			 "expicitly stated\n");
	    for (i = 0; i < MAX_IO_METHOD; i++)
		test_params_p->io_method[i] = TRUE;
	}
    }

    /* Two cases - Either use the default test parameters or the
     * single test mode, but at least one of these must produce a
     * viable test */
    
    test_single_used = 0;
    test_arr_used = 0;
    for (i = 0; i < MAX_RW; i++)
    {
	if (test_params_p->rw[i] == TRUE)
	{
	    for (j = 0; j < MAX_TEST; j++)
	    {
		if (j != BAND_SINGLE && j != CHECK_SINGLE)
		{
		    if (test_params_p->test[j] == TRUE)
			for (k = 0; k < MAX_NONCONTIG; k++)
			    if (test_params_p->noncontig[k] == TRUE)
				for (l = 0; l < MAX_IO_METHOD; l++)
				    if (test_params_p->io_method[l] == 
					TRUE)
					test_arr_used = TRUE;
		}
		else
		{
		    if (test_params_p->test[j] == TRUE)
			for (k = 0; k < MAX_NONCONTIG; k++)
			    if (test_params_p->noncontig[k] == TRUE)
				for (l = 0; l < MAX_IO_METHOD; l++)
				    if (test_params_p->io_method[l] == 
					TRUE)
					test_single_used = TRUE;
		}
	    }
	}
    }

    /* Make sure that single test mode and test array are not
     * simultaneously on */
    if (test_arr_used == TRUE && test_single_used == TRUE)
    {
	debug_stderr("** Error: Test arrays (-b,-x) and test single "
		     "modes (-B,-X) are both in use.  Use test arrays or "
		     "a single test mode, but not both at the same time.\n");
	return -1;
    }
    else if (test_arr_used == TRUE && test_single_used == FALSE)
    {
	test_params_p->test[BAND_SINGLE] = FALSE;
	test_params_p->test[CHECK_SINGLE] = FALSE;
    }
    else if (test_arr_used == FALSE && test_single_used == TRUE)
    {
	if (test_params_p->test[BAND_SINGLE] == TRUE &&
	    test_params_p->test[CHECK_SINGLE] == TRUE)
	{
	    debug_stderr("** Error: Test bandwidth single mode and test check"
			 "single mode (-B,-X) are both in use.  Use either "
			 "-B or -X, but not both at the same time.\n");
	    return -1;
	}
	if (test_params_p->test[BAND_SINGLE] == TRUE)
	    test_params_p->test[CHECK_SINGLE] = FALSE;
	else
	    test_params_p->test[BAND_SINGLE] = FALSE;

	for (i = 0; i < MAX_TEST; i++)
	{
	    if (i != BAND_SINGLE && i != CHECK_SINGLE)
		test_params_p->test[i] = FALSE;
	}
    }

    /* Look for the particular check test */
    if (test_params_p->test[CHECK_SINGLE] == TRUE)
    {
	int found = 0;
	for (i = 0; i < MAX_HUMAN_COUNT; i++)
	{
	    if (strlen(test_params_p->def_check_test) ==
		strlen(human_name[i])) 
	    {
		if (strcmp(test_params_p->def_check_test,
			   human_name[i]) == 0)
		{
		    test_params_p->def_check_test_num = HUMAN;
		    test_params_p->def_check_test_param_val = i;
		    found = 1;
		    break;
		}
	    }
	}
	if (found == 0)
	{
	    for (i = 0; i < MAX_DEFINED_COUNT; i++)
	    {
		if (strlen(test_params_p->def_check_test) ==
		    strlen(defined_name[i])) 
		{
		    if (strcmp(test_params_p->def_check_test,
			       defined_name[i]) == 0)
		    {
			test_params_p->def_check_test_num = DEFINED;
			test_params_p->def_check_test_param_val = i;
			found = 1;
			break;
		    }
		}
	    }
	}
	if (found == 0)
	{
	    debug_stderr("Error - %s not found in any check test.  Fix -X "
			 "option.\n", test_params_p->def_check_test);
	    return -1;
	}
    }

    /* Make sure that a test is selected */
    if (test_single_used != TRUE && test_arr_used != TRUE)
    {
	int tmp_used = FALSE;
	tmp_used = FALSE;
	debug_stderr("\nNot enough test parameters selected.  Either "
		     "choose tests arrays (-b,-x), single test modes "
		     "(-B,-X), or estimate space mode (-E).  If using "
		     "tests arrays (-b,-x) single test modes (-B,-X), "
		     "there exist several requirements including...\n");
	for (i = 0; i < MAX_RW; i++)
	{
	    if (test_params_p->rw[i] == TRUE)
		tmp_used = TRUE;
	}
	if (tmp_used == FALSE)
	    debug_stderr("** Requires at least WRITE (-O option)\n");
	tmp_used = FALSE;
	for (i = 0; i < MAX_NONCONTIG; i++)
	{
	    if (test_params_p->noncontig[i] == TRUE)
		tmp_used = TRUE;
	}
	if (tmp_used == FALSE)
	    debug_stderr("** Requires at least one noncontig type "
			 "(-n option)\n");
	tmp_used = FALSE;
	for (i = 0; i < MAX_IO_METHOD; i++)
	{
	    if (test_params_p->io_method[i] == TRUE)
		tmp_used = TRUE;
	}
	if (tmp_used == FALSE)
	    debug_stderr("** Requires at least one I/O method "
			 "(-m option)\n");
	debug_stderr("\n");
	print_usage(argv[0]);
	return -1;
    }

    if (test_params_p->enable_resume == TRUE)
    {
	if (test_params_p->rw[test_params_p->resume_arr[VAR_RW]] != TRUE)
	{
	    debug_stderr("** Enable resume of %s is not selected as part "
			 "of testing\n", rw_name[
			     test_params_p->resume_arr[VAR_RW]]);
	    print_usage(argv[0]);
	    return -1;
	}
	if (test_params_p->test[test_params_p->resume_arr[VAR_TEST]] != TRUE)
	{
	    debug_stderr("** Enable resume of %s is not selected as part "
			 "of testing\n", test_type_name[
			     test_params_p->resume_arr[VAR_TEST]]);
	    print_usage(argv[0]);
	    return -1;
	}
	if (test_params_p->noncontig[
		test_params_p->resume_arr[VAR_NONCONTIG]] != TRUE)
	{
	    debug_stderr("** Enable resume of %s is not selected as part "
			 "of testing\n", noncontig_type_name[
			     test_params_p->resume_arr[VAR_NONCONTIG]]);
	    print_usage(argv[0]);
	    return -1;
	}
	if (test_params_p->io_method[
		test_params_p->resume_arr[VAR_IO_METHOD]] != TRUE)
	{
	    debug_stderr("** Enable resume of %s is not selected as part "
			 "of testing\n", io_method_name[
			     test_params_p->resume_arr[VAR_IO_METHOD]]);
	    print_usage(argv[0]);
	    return -1;
	}
    }

    /* Get debugging options and set them to be passed around */
    debug_env = getenv("DEBUG_MASK");
    if (debug_env != NULL)
	test_params_p->debug_mask = debug_convert_to_mask(debug_env);
    else
    {
	test_params_p->debug_mode = DEBUG_OFF;
	test_params_p->debug_mask = 0;
    }
    if (test_params_p->debug_mask > 0)
	test_params_p->debug_mode = DEBUG_ON;

    return 0;
}    

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
